typedef unsigned __int64 UINT64;
typedef int INT;
typedef unsigned UINT;
typedef int BOOL;

UINT64 Long_Plus(UINT64 a, UINT64 b)
{
	return (a + b);
}

UINT64 Long_Times(UINT64 a, UINT64 b)
{
	return (a * b);
}

UINT64 Long_Minus(UINT64 a, UINT64 b)
{
	return (a - b);
}

UINT64 Long_Divide(UINT64 a, UINT64 b)
{
	return (a / b);
}

UINT64 Long_Mod(UINT64 a, UINT64 b)
{
	return (a % b);
}

BOOL Long_LT(UINT64 a, UINT64 b)
{
	return (a < b);
}

BOOL Long_LE(UINT64 a, UINT64 b)
{
	return (a <= b);
}

BOOL Long_GT(UINT64 a, UINT64 b)
{
	return (a > b);
}

BOOL Long_GE(UINT64 a, UINT64 b)
{
	return (a >= b);
}

UINT64 Long_And(UINT64 a, UINT64 b)
{
	return (a & b);
}

UINT64 Long_Or(UINT64 a, UINT64 b)
{
	return (a | b);
}

UINT64 Long_Xor(UINT64 a, UINT64 b)
{
	return (a ^ b);
}

UINT64 Long_Not(UINT64 a)
{
	return (!a);
}

UINT64 Long_Shift(UINT64 a, INT b)
{
	if (b >= 0)
		return (a << b);
	else
		return (a >> -b);
}

UINT64 Long_LeftShift(UINT64 a, UINT b)
{
	return (a << b);
}

UINT64 Long_RightShift(UINT64 a, UINT b)
{
	return (a >> b);
}

UINT64 Long_Rotate(UINT64 a, INT b)
{
	if (b >= 0)
		return ((a << b) | (a >> (64 - b)));
	else
	{
		b = -b;
		return ((a >> b) | (a << (64 - b)));
	}
}

UINT64 Long_LeftRotate(UINT64 a, UINT b)
{
	return ((a << b) | (a >> (64 - b)));
}

UINT64 Long_RightRotate(UINT64 a, UINT b)
{
	return ((a >> b) | (a << (64 - b)));
}

UINT64 Long_Extract(UINT64 a, UINT i, UINT n)
{
	if ((i + n) > 64)
	{
		/* checked runtime error */
	}
	/* or is it 64 - n? */
	return ((a >> i) & (~(~(UINT64)0)) << n);
}

UINT64 Long_Insert(UINT64 a, UINT64 b, UINT i, UINT n)
{
	UINT64 Mask;
	if ((i + n) > 64)
	{
		/* checked runtime error */
	}
	/* or is it 64 - n? */
	Mask = (((~(~(UINT64)0)) << n) << i);
	return ((a & Mask) | ((b << i) & Mask));
}
