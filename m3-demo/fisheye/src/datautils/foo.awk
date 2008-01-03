From mhb@Pa.dec.com Thu Sep 26 02:28:26 1991
Return-Path: <mhb@Pa.dec.com>
Received: from inet-gw-1.pa.dec.com by cs.brown.edu (5.64+/Doorknob-1.3)
	id AA25463; Thu, 26 Sep 91 02:28:21 -0400
Received: by inet-gw-1.pa.dec.com; id AA14031; Wed, 25 Sep 91 23:28:10 -0700
Received: by jumbo.pa.dec.com; id AA02107; Wed, 25 Sep 91 23:28:07 -0700
From: mhb@Pa.dec.com (Marc H. Brown)
Message-Id: <9109260628.AA02107@jumbo.pa.dec.com>
Date: Wed, 25 Sep 91 23:27:59 PDT
To: ms@cs.brown.edu (Manojit Sarkar)
X-Folder-Carbon: mhbcc
Subject: by the way, 
In-Reply-To: Message of Thu, 26 Sep 91 02:13:58 -0400
    from ms@cs.brown.edu (Manojit Sarkar)
    <9109260613.AA29434@gorby.cs.brown.edu>
Status: R



here's the 40 line program that you've resisted writing for over a
month:

BEGIN { 
    v = stateCt = -1; 
    printf "xxxx\n"
    }

/{/ {
    state = $1
    }
     
/moveto/ { 
    v++; stateCt++;
    stateX = $1; stateY = $2
    printf "%s%d\n", state, stateCt
    printf "%06d.0 %06d.0 1.0\n", $1, $2
    printf "Edge\n"
    printf "%04d 00\n", v+1
    printf "\n"
    }

/lineto/ {
    v++;
    printf "vertex%d\n", v
    printf "%06d.0 %06d.0 1.0\n", $1, $2
    printf "Edge\n"
    printf "%04d 00\n", v+1
    printf "\n"
    }

/closepath/ {
    v++;
    printf "fake%d\n", v
    printf "%06d.0 %06d.0 1.0\n", stateX, stateY
    printf "\n"
    }
    
END { 
    printf "number of vertices = %d\n", v+1
    }


to run it, "awk -f foo.awk <map.ps >map.out", where the 40 lines are
stored in the file called foo.awk

