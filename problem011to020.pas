UNIT problem011to020;

INTERFACE

USES Euler, Classes, Sysutils, Strutils, Math, Velthuis.BigIntegers;
FUNCTION Problem011: Integer;
FUNCTION Problem012: Integer;
FUNCTION Problem013: STRING;
FUNCTION Problem014: Uint64;
FUNCTION Problem015: Uint64;
FUNCTION Problem016: Integer;
FUNCTION Problem017: Integer;
FUNCTION Problem018: Integer;
FUNCTION Problem019: Integer;
FUNCTION Problem020: Integer;

IMPLEMENTATION

FUNCTION Problem011: Integer;

CONST C_FNAME =
    'C:\Users\bvbes\OneDrive\Documents\GitHub\ProjectEulerDelphi\Problem_11.txt';

VAR Numbers: TStringList; Matrix: ARRAY [1 .. 20, 1 .. 20] OF Integer;
  X, Y, Count, Max, Temp: Longint;

BEGIN
  Numbers := TStringList.Create;
  Numbers.Delimiter := Char(' ');
  TRY Numbers.LoadFromFile(C_FNAME);
  EXCEPT
    ON E: EInOutError DO
        Writeln('File handling error occured. Reason: ', E.Message);
  END;
  Numbers.DelimitedText := Numbers.Text;
  Count := 0;
  FOR X := 1 TO 20 DO
    FOR Y := 1 TO 20 DO BEGIN
      Matrix[X, Y] := StrToInt(Numbers[Count]);
      Inc(Count);
    END;
  Max := 0;

  // horizontals -
  FOR X := 1 TO 20 DO BEGIN
    FOR Y := 1 TO 17 DO BEGIN
      Temp := Matrix[X, Y] * Matrix[X, Y + 1] * Matrix[X, Y + 2] *
        Matrix[X, Y + 3];
      IF Temp > Max THEN Max := Temp;
    END;
  END;

  // vertical |
  FOR X := 1 TO 17 DO BEGIN
    FOR Y := 1 TO 20 DO BEGIN
      Temp := Matrix[X, Y] * Matrix[X + 1, Y] * Matrix[X + 2, Y] *
        Matrix[X + 3, Y];
      IF Temp > Max THEN Max := Temp;
    END;
  END;

  // Dia \
  FOR X := 1 TO 17 DO BEGIN
    FOR Y := 1 TO 17 DO BEGIN
      Temp := Matrix[X, Y] * Matrix[X + 1, Y + 1] * Matrix[X + 2, Y + 2] *
        Matrix[X + 3, Y + 3];
      IF Temp > Max THEN Max := Temp;
    END;
  END;

  // Dia /
  FOR X := 1 TO 17 DO BEGIN
    FOR Y := 4 TO 20 DO BEGIN
      Temp := Matrix[X, Y] * Matrix[X + 1, Y - 1] * Matrix[X + 2, Y - 2] *
        Matrix[X + 3, Y - 3];
      IF Temp > Max THEN Max := Temp;
    END;
  END;
  Numbers.Free;
  Result := Max;
END;

FUNCTION Problem012: Integer;
VAR I: Integer;

BEGIN
  Result := 0;
  I := 1;
  WHILE (NumberOfDivisors(Result) < 500) DO BEGIN
    Inc(Result, I);
    Inc(I);
  END;
END;

FUNCTION Problem013: STRING;
CONST C_FNAME =
    'C:\Users\bvbes\OneDrive\Documents\GitHub\ProjectEulerDelphi\Problem_13.txt';
VAR Numbers: TStringList; X: STRING; Sum: Biginteger;

BEGIN
  Sum := 0;
  Numbers := TStringList.Create;
  TRY Numbers.LoadFromFile(C_FNAME);
  EXCEPT
    ON E: EInOutError DO
        Writeln('File handling error occured. Reason: ', E.Message);
  END;

  FOR X IN Numbers DO Sum := Sum + X;

  Result := Leftstr(Sum.Tostring, 10);
END;

FUNCTION Problem014: Uint64;
VAR CurrentLength, MaxLength, MaxStartingNumber, I, N: Uint64;
BEGIN
  MaxLength := 0;
  MaxStartingNumber := 0;
  FOR I := 1 TO 1000000 DO BEGIN
    CurrentLength := 1;
    N := I;
    WHILE N > 1 DO BEGIN
      IF (N MOD 2) = 0 THEN // if n is even
          N := N DIV 2
      ELSE // if n is odd
          N := 3 * N + 1;
      Inc(CurrentLength);
    END;
    IF CurrentLength > MaxLength THEN BEGIN
      MaxLength := CurrentLength;
      MaxStartingNumber := I;
    END;
  END;
  Result := MaxStartingNumber;
END;

FUNCTION Problem015: Uint64;
CONST GridSize = 20;
VAR Grid: ARRAY [0 .. GridSize, 0 .. GridSize] OF Uint64; I, J: Integer;
BEGIN
  // Initialize the grid borders with 1's
  FOR I := 0 TO GridSize DO BEGIN
    Grid[I, 0] := 1;
    Grid[0, I] := 1;
  END;

  // Compute the number of paths for each cell in the grid
  FOR I := 1 TO GridSize DO BEGIN
    FOR J := 1 TO GridSize DO BEGIN
      Grid[I, J] := Grid[I - 1, J] + Grid[I, J - 1];
    END;
  END;

  // The solution is the number of paths from the top-left to the bottom-right corner
  Result := Grid[GridSize, GridSize];
END;

FUNCTION Problem016: Integer;
VAR Number: Biginteger;

BEGIN
  Number := Biginteger.Pow(2, 1000);
  Result := GetSumOfDigits(Number.Tostring);
END;

FUNCTION Problem017: Integer;
VAR Arr: ARRAY [0 .. 1000] OF Integer;
  I, Tens, Ones, Hundreds, Tens_ones, Sum: Integer;

BEGIN
  FOR I := 0 TO 1000 DO Arr[I] := 0;
  Sum := 0;
  Arr[0] := Length('');
  Arr[1] := Length('one');
  Arr[2] := Length('two');
  Arr[3] := Length('three');
  Arr[4] := Length('four');
  Arr[5] := Length('five');
  Arr[6] := Length('six');
  Arr[7] := Length('seven');
  Arr[8] := Length('eight');
  Arr[9] := Length('nine');
  Arr[10] := Length('ten');
  Arr[11] := Length('eleven');
  Arr[12] := Length('twelve');
  Arr[13] := Length('thirteen');
  Arr[14] := Length('fourteen');
  Arr[15] := Length('fifteen');
  Arr[16] := Length('sixteen');
  Arr[17] := Length('seventeen');
  Arr[18] := Length('eighteen');
  Arr[19] := Length('nineteen');
  Arr[20] := Length('twenty');
  Arr[30] := Length('thirty');
  Arr[40] := Length('forty');
  Arr[50] := Length('fifty');
  Arr[60] := Length('sixty');
  Arr[70] := Length('seventy');
  Arr[80] := Length('eighty');
  Arr[90] := Length('ninety');
  Arr[1000] := Length('onethousand');

  FOR I := 21 TO 99 DO BEGIN
    Tens := (I DIV 10) * 10;
    Ones := I - Tens;
    Arr[I] := Arr[Tens] + Arr[Ones];
  END;
  FOR I := 100 TO 999 DO BEGIN
    Hundreds := I DIV 100;
    Tens_ones := I - Hundreds * 100;
    // if the value of tens and ones place is 0
    // just use 'hundred' instead of 'and hundred'
    IF Tens_ones = 0 THEN Arr[I] := Arr[Hundreds] + Length('hundred')
    ELSE Arr[I] := Arr[Hundreds] + Length('onehundred') + Arr[Tens_ones];
  END;

  FOR I := 0 TO 1000 DO Inc(Sum, Arr[I]);
  Result := Sum;
END;

FUNCTION Problem018: Integer;
CONST C_FNAME =
    'C:\Users\bvbes\OneDrive\Documents\GitHub\ProjectEulerDelphi\Problem_18.txt';
  Lines = 15;

VAR SlInfo: TStringList; Arr: ARRAY [1 .. Lines, 1 .. Lines] OF Integer;
  I, X, Y: Longint;

BEGIN
  SlInfo := TStringList.Create;
  SlInfo.Delimiter := Char(' ');
  TRY SlInfo.LoadFromFile(C_FNAME);
  EXCEPT
    ON E: EInOutError DO
        Writeln('File handling error occurred. Reason: ', E.Message);
  END;
  SlInfo.DelimitedText := SlInfo.Text;
  // populate matrix
  I := 0;
  FOR X := 1 TO Lines DO
    FOR Y := 1 TO X DO BEGIN
      Arr[X, Y] := StrToInt(SlInfo[I]);
      Inc(I);
    END;
  // count from the bottom up
  FOR X := Lines - 1 DOWNTO 1 DO
    FOR Y := 1 TO X DO
        Arr[X, Y] := Arr[X, Y] + Max(Arr[X + 1, Y], Arr[X + 1, Y + 1]);
  Result := Arr[1, 1];
  SlInfo.Free;
END;

FUNCTION Problem019: Integer;
VAR Year, Month: Word;

BEGIN
  Result := 0;
  FOR Year := 1901 TO 2000 DO
    FOR Month := 1 TO 12 DO
      IF Dayofweek(Encodedate(Year, Month, 1)) = 1 THEN Inc(Result);
END;

FUNCTION Problem020: Integer;
  FUNCTION Factorial(N: Integer): Biginteger;
  VAR I: Integer;
  BEGIN
    Result := 1;
    FOR I := 2 TO N DO Result := Result * I;
  END;

VAR Digits: STRING; DigitSum: Integer; FactResult: Biginteger;
BEGIN
  FactResult := Factorial(100);
  Digits := FactResult.Tostring;

  Result := GetSumOfDigits(Digits);
END;

END.
