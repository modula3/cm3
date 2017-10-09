(* Copyright (C) 2017 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

MODULE Haar;

IMPORT Image,Classifier,Rectangles;
IMPORT Word,RefSeq,Math,Thread;
IMPORT Fmt,IO;

CONST
  NumFeatures = Classifier.NumFeatures;
  WinSize = 24;
  GroupOverlap = 0.4E0; (* group overlaping windows *)
  
REVEAL 

  T = Public BRANDED "Haar.T" OBJECT
    minSize,maxSize : Size;
    origWindowSize : Size;  (* size of the window in the training set [24x24] *)
    invWindowArea : INTEGER; (* check - its not really the inverse *)
    factor : REAL; (* the scale factor we calc and pass to a thread *)
    image : Image.Image; (* keep ref for threads *)
    cl : Classifier.Class; (* the classifier we read in *)
    equRect : Rectangles.Rect; (* this is bit bogus just a copy of origwindowsize*)    
    matches : RefSeq.T; (* any results we keep here protected by a mutex *)
  OVERRIDES
    init := Init;
    detectObjects := DetectObjects;
  END;
  
TYPE
  Byte = [0..255];
  
  Point = RECORD
    x,y : INTEGER;
  END;

  ScaleRec = RECORD
    sz : Size;
    factor : REAL;
  END;
  
  RefScale = REF ARRAY OF ScaleRec;
  
  HaarClosure = Thread.Closure OBJECT
    id : CARDINAL;
    width,height : INTEGER; (* orig image width and height *) 
    factor : REAL; (* scale factor for this thread *)
    sz : Size; (* scaled image size after modified by factor *)
    sum : Image.Image; (* integral image *)
    sqsum : Image.Image; (* square of integral image *) 
    obj : T;
  METHODS
   nearestNeighbour(READONLY src : Image.Image; 
                    VAR dst : Image.Image) := NearestNeighbour;    
   integralImages(READONLY src : Image.Image) := IntegralImages;
   scaleImageInvoker(factor : REAL; sumRow,sumCol : INTEGER) := ScaleImageInvoker;
   cascadeClassifier(pt : Point) : INTEGER := CascadeClassifier;
   evalWeakClassifier(varianceNormFactor,nodeIdx : INTEGER; pt : Point ) : INTEGER := EvalWeakClassifier;
  OVERRIDES
    apply := HaarThread;
  END;

  RefClosure = REF ARRAY OF HaarClosure;
  RefThread = REF ARRAY OF Thread.T;
  
VAR
  resultMu := NEW(MUTEX);
  haarClosure : RefClosure;
  threads : RefThread;
  sizes := NEW(RefScale, 10);
  
PROCEDURE Init(self : T; infoFile,classFile : TEXT; minSize,maxSize : Size := Size{0,0}) : T =
  BEGIN

    TRY
      self.cl := Classifier.ReadClassifiers(infoFile,classFile);
    EXCEPT
    | Classifier.ClassError => RETURN NIL;
    END;
    
    (* maxsize is 0 to reset it to image size later *)
    self.minSize := minSize;
    self.maxSize := maxSize;

    self.origWindowSize.height := WinSize;
    self.origWindowSize.width := WinSize;
    
    self.equRect.x := 0;
    self.equRect.y := 0;
    self.equRect.width := self.origWindowSize.width;
    self.equRect.height := self.origWindowSize.height;

    self.invWindowArea := self.equRect.width * self.equRect.height;
    DEC(self.equRect.width);
    DEC(self.equRect.height);
    
    RETURN self;
  END Init;

PROCEDURE Expand () =
  VAR
    n   := NUMBER (sizes^);
    new := NEW (RefScale, n + n);
  BEGIN
    SUBARRAY (new^, 0, n) := sizes^;
    sizes := new;
  END Expand;
  
(* Downsample an image using nearest neighbour *)
    
PROCEDURE NearestNeighbour(<*UNUSED*>self : HaarClosure; READONLY src : Image.Image; VAR dst : Image.Image) =
  VAR
    x,y,rat,xRatio,yRatio : INTEGER;
  BEGIN
    rat := 0;
    
    xRatio := (Word.LeftShift(src.width,16) DIV dst.width) + 1;
    yRatio := (Word.LeftShift(src.height,16) DIV dst.height) + 1;
  
    FOR i := 0 TO dst.height - 1 DO
      y := Word.RightShift(i * yRatio,16);
      rat := 0;
      FOR j := 0 TO dst.width - 1 DO
        x := Word.RightShift(rat,16);
        dst.data[i,j] := src.data[y, x];
        INC(rat,xRatio); 
      END;
    END;
  END NearestNeighbour;
  
(* Compute the integral image (and squared integral)
 * Integral image helps quickly sum up an area.
 * More info:
 * http://en.wikipedia.org/wiki/Summed_area_table
 ****************************************************)

PROCEDURE IntegralImages(self : HaarClosure; READONLY src : Image.Image) =
  VAR
    s,sq,t,tq : INTEGER;
    b : Byte;
  BEGIN

    FOR y := 0 TO src.height - 1 DO
      s := 0; sq := 0;
      FOR x := 0 TO src.width - 1 DO
        b := src.data[y,x];
        s := s + b;
        sq := sq + b * b;
        t := s; tq := sq;
        IF y > 0 THEN
          t := t + self.sum.data[y-1,x];
          tq := tq + self.sqsum.data[y-1,x];
        END;
        self.sum.data[y,x] := t;
        self.sqsum.data[y,x] := tq;
      END;
    END;
  END IntegralImages;
  
PROCEDURE ScaleImageInvoker(self : HaarClosure; factor : REAL; sumRow,sumCol : INTEGER) =
  VAR
    p : Point;
    r : Rectangles.RefRect;
    result,y2,x2,step : INTEGER;
    winSize0,winSize : Size;
  BEGIN
    winSize0 := self.obj.origWindowSize;
    winSize.width := ROUND(FLOAT(winSize0.width,REAL) * factor);
    winSize.height := ROUND(FLOAT(winSize0.height,REAL) * factor);
    
    (* When filter window shifts to image boarder,
     * some margin need to be kept *)
    y2 := sumRow - winSize0.height;
    x2 := sumCol - winSize0.width;

    (* Step size of filter window shifting. Reducing step makes program faster,
     * but decreases quality of detection.
     * example: step = factor > 2 ? 1 : 2;
     * 
     * the factor and step can be kept constant, unless you want to change input image.
     *
     * The step size is set to 1 here, i.e., shift the filter window by 1 pixel. *)
    
    step := 1;
     
    (**********************************************
     * Shift the filter window over the image.
     * Each shift step is independent.
     *********************************************)

    FOR x := 0 TO x2 - 1 BY step DO
      FOR y := 0 TO y2 - 1 BY step DO
        p.x := x;
        p.y := y;
       
        result := self.cascadeClassifier(p);

       (* If a face is detected, record the coordinates of the filter window
        * Note that, if the filter runs on GPUs,
        * the push_back operation is not possible on GPUs.
        * The GPU may need to use a simpler data structure,
        * e.g., an array, to store the coordinates of face,
        * which can be later memcpy from GPU to CPU to do push_back
        *******************************************************)
        IF result > 0 THEN
          r := NEW(Rectangles.RefRect);
          r.x := ROUND(FLOAT(x,REAL) * factor);
          r.y := ROUND(FLOAT(y,REAL) * factor);
          r.width := winSize.width;
          r.height := winSize.height;

          LOCK resultMu DO
            self.obj.matches.addhi(r);
          END;
        END;
      END

    END;
  END ScaleImageInvoker;
  
(* runs the cascade on the specified window *)

PROCEDURE CascadeClassifier(self : HaarClosure; pt : Point) : INTEGER =
  VAR
    mean,varianceNormFactor,res : INTEGER;
    stageSum,nodeIdx : INTEGER := 0;
  BEGIN

    (* Image normalization
     * mean is the mean of the pixels in the detection window
     * cascade.sqsum are the squared pixel values (using the squared integral image)
     * invWindowArea is 1 over the count of pixels in the detection window *)

    WITH s = self.sum.data, 
         sq = self.sqsum.data,
         eq = self.obj.equRect DO
         
      varianceNormFactor := sq[pt.y, pt.x] -
                            sq[pt.y, pt.x + eq.width] -
                            sq[pt.y + eq.height, pt.x] +
                            sq[pt.y + eq.height, pt.x + eq.width];

      mean := s[pt.y, pt.x] -
              s[pt.y, pt.x + eq.width] -
              s[pt.y + eq.height, pt.x] +
              s[pt.y + eq.height, pt.x + eq.width];
    END;
    
    varianceNormFactor := Word.And(varianceNormFactor * self.obj.invWindowArea,16_FFFFFFFF);

    varianceNormFactor :=  Word.And(varianceNormFactor - mean * mean,16_FFFFFFFF);
    IF varianceNormFactor > 0 THEN
      varianceNormFactor := TRUNC(Math.sqrt(FLOAT(varianceNormFactor,LONGREAL)));
    ELSE
      varianceNormFactor := 1;
    END;
    
    (**************************************************
     * The major computation happens here.
     * For each scale in the image pyramid,
     * and for each shifted step of the filter,
     * send the shifted window through cascade filter.
     *
     * Note:
     *
     * Stages in the cascade filter are independent.
     * However, a face can be rejected by any stage.
     * Running stages in parallel delays the rejection,
     * which induces unnecessary computation.
     *
     * Filters in the same stage are also independent,
     * except that filter results need to be merged,
     * and compared with a per-stage threshold.
     *************************************************)
   
    FOR i := 0 TO self.obj.cl.stageCnt - 1 DO
      stageSum := 0;
      FOR j := 0 TO self.obj.cl.stages[i] - 1 DO
      
        (* Send the shifted window to a haar filter. *)
        res := self.evalWeakClassifier(varianceNormFactor, nodeIdx, pt);

        INC(stageSum,res);        
        INC(nodeIdx);
      END;

      (**************************************************************
       * threshold of the stage. 
       * If the sum is below the threshold, 
       * no faces are detected, 
       * and the search is abandoned at the i-th stage (-i).
       * Otherwise, a face is detected (1)
       **************************************************************)

      (* the number "0.4" is empirically chosen for this example *)

      IF FLOAT(stageSum) < 0.4E0 * FLOAT(self.obj.cl.stagesThresh[i]) THEN
        RETURN -i;
      END;
    END; (* stage loop *)
   
    RETURN 1;
  END CascadeClassifier;
  
(****************************************************
 * evalWeakClassifier:
 * the actual computation of a haar filter.
 * More info:
 * http://en.wikipedia.org/wiki/Haar-like_features
 ***************************************************)

PROCEDURE EvalWeakClassifier(self : HaarClosure; varianceNormFactor,nodeIdx : INTEGER; pt : Point ) : INTEGER =
  VAR
    t,sum,fsum,x,y : INTEGER;
  BEGIN
    t := self.obj.cl.nodes[nodeIdx].treeThresh * varianceNormFactor;

    sum := 0;
    FOR k := 0 TO NumFeatures - 1 DO
      WITH f = self.obj.cl.nodes[nodeIdx].feature[k],
           tr = f.rect,
           data = self.sum.data DO

        IF k = NumFeatures - 1 AND tr.x = 0 AND tr.y = 0 THEN
          (* third feature usually 0 *)
          fsum := 0;
        ELSE
          x := tr.x + pt.x;
          y := tr.y + pt.y;
          fsum := data[y, x] - 
                  data[y, x + tr.width] - 
                  data[y + tr.height, x] + 
                  data[y + tr.height, x + tr.width];
          fsum := fsum * f.weight;
        END;
      END;
      INC(sum,fsum);
    END;

    IF sum >= t THEN
      RETURN self.obj.cl.nodes[nodeIdx].alpha2;
    ELSE
      RETURN self.obj.cl.nodes[nodeIdx].alpha1;
    END;
  END EvalWeakClassifier;
  
PROCEDURE HaarThread (self : HaarClosure): REFANY =
  VAR
    scaledImage : Image.Image;
  BEGIN

    scaledImage := Image.CreateImage(self.sz.width, self.sz.height);  
    self.sum := Image.CreateImage(self.sz.width, self.sz.height);  
    self.sqsum := Image.CreateImage(self.sz.width, self.sz.height);
    
    Image.SetImage(self.sz.width, self.sz.height, scaledImage);
    Image.SetImage(self.sz.width, self.sz.height, self.sum);
    Image.SetImage(self.sz.width, self.sz.height, self.sqsum);
  
    (* Compute-intensive step: building image pyramid by downsampling
       using nearest neighbour *)
         
    self.nearestNeighbour(self.obj.image, scaledImage);
      
    (* Compute-intensive step: At each scale of the image pyramid,
     * compute a new integral and squared integral image *)
         
    self.integralImages(scaledImage);
      
    (* 
     * Note:
     * Summing pixels within a haar window is done by
     * using four corners of the integral image:
     * http://en.wikipedia.org/wiki/Summed_area_table *)
    
    (* Process the current scale with the cascaded filter.
     * The main computations are invoked by this function.
     * Optimization opportunity:
     * the same cascade filter is invoked each time *)

    self.scaleImageInvoker(self.factor, self.sum.height, self.sum.width);
    RETURN NIL;
  END HaarThread;

PROCEDURE DetectObjects(self : T; 
            READONLY image : Image.Image;                        
            scaleFactor : REAL;
            minNeighbours : INTEGER) : RefSeq.T =
  VAR
    iter : INTEGER := 0;
    winSize,winSize0,sz,sz1 : Size;
  BEGIN
    (* save image for threads *)
    self.image := image;
    
    self.factor := 1.0E0;
    self.matches := NEW(RefSeq.T).init();
  
    IF self.maxSize.height = 0 OR self.maxSize.width = 0 THEN
      self.maxSize.height := image.height;
      self.maxSize.width := image.width;
    END;
    
    (* window size of the training set *)
    winSize0 := self.origWindowSize;

    (* iterate over the image pyramid *)
    LOOP
  
      (* size of the image scaled up *)
      winSize.width := ROUND(FLOAT(winSize0.width,REAL) * self.factor);
      winSize.height := ROUND(FLOAT(winSize0.height,REAL) * self.factor);

      (* size of the image scaled down (from bigger to smaller) *)
      sz.width := TRUNC(FLOAT(image.width,REAL) / self.factor);
      sz.height := TRUNC(FLOAT(image.height,REAL) / self.factor);
      
      (* difference between sizes of the scaled image and the original detection window *)
      sz1.width := sz.width - winSize0.width;
      sz1.height := sz.height - winSize0.height;
  
      (* if the actual scaled image is smaller than the original detection window, *)
      IF sz1.width < 0 OR sz1.height < 0 THEN
        EXIT;
      END;

      (* if a minSize different from the original detection window is specified,   
         continue to the next scaling *)
      IF NOT (winSize.width < self.minSize.width OR 
              winSize.height < self.minSize.height) THEN

        IF iter >= NUMBER (sizes^) THEN Expand (); END;
              
        sizes[iter].sz := sz;
        sizes[iter].factor := self.factor;

      END;
    
      self.factor := self.factor * scaleFactor;
      INC(iter);
    END;
    IO.Put("iter " & Fmt.Int(iter) &  "\n");

    (* create the threads *)
    haarClosure := NEW(RefClosure, iter);
    threads := NEW(RefThread, iter);
    FOR i := 0 TO iter - 1 DO
      haarClosure[i] := NEW(HaarClosure, obj := self, id := iter, sz := sizes[i].sz, width := image.width, height := image.height, factor := sizes[i].factor);
      threads[i] := Thread.Fork(haarClosure[i]);     
    END;
        
    (* join from last to first as the last ones have a smaller window size
       and should finish first *)
    FOR i := iter - 1 TO 0 BY -1 DO
      EVAL Thread.Join(threads[i]);
    END;
    
    IF minNeighbours # 0 THEN
      Rectangles.GroupRectangles(self.matches, minNeighbours, GroupOverlap);
    END;

    RETURN self.matches;
  
  END DetectObjects;
  
BEGIN
END Haar.
