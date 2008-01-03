
CREATE TABLE types (
	text	text,
	varchar20  varchar(20),
	varchar80  varchar(80),
	char4	   char(4),
	real	   real,
	float	   float,
	date	   date,
	time	   time,
	int	   int,
	smallint   smallint)

INSERT INTO types
   VALUES ('atext', '20var', '80var', 'abcd', 10.0, 20.0, 
	   '10/20/1996', '20:30:20', 20, 30)
	
