UNIT problem041to050;

INTERFACE

USES Euler, Sysutils, Classes, Math, System.Generics.Collections,
  Velthuis.BigIntegers;
FUNCTION Problem041: STRING;
FUNCTION Problem042: Integer;
FUNCTION Problem043: Uint64;
FUNCTION Problem044: Integer;
FUNCTION Problem045: Uint64;
FUNCTION Problem046: Uint32;
FUNCTION Problem047: Uint32;
FUNCTION Problem048: STRING;
FUNCTION Problem049: STRING;
FUNCTION Problem050: Uint32;

IMPLEMENTATION

FUNCTION Problem041: STRING;
CONST Max = 7654321;

TYPE
  TThingRec = RECORD
    Ch: Char;
    Occ: Boolean;
  END;

  TDynBools = TArray<Boolean>;

VAR Thing: ARRAY [1 .. 255] OF TThingRec; EntryString: STRING;
  Primestrs: TStringList; Is_prime: TDynBools;

  PROCEDURE Permutate(Num: Byte);

  VAR I: Byte; Prim: STRING;
  BEGIN
    IF Num > Length(EntryString) THEN BEGIN
      Prim := '';
      FOR I := 1 TO Length(EntryString) DO Prim := Prim + Thing[I].Ch;
      Primestrs.Add(Prim);
    END
    ELSE
      FOR I := 1 TO Length(EntryString) DO BEGIN
        IF (NOT Thing[I].Occ) THEN BEGIN
          Thing[I].Occ := True;
          Thing[I].Ch := EntryString[Num];
          Permutate(Succ(Num));
          Thing[I].Occ := False;
        END;
      END;
  END;

VAR I: Longint;
BEGIN
  Is_prime := Sieve(Max);
  Primestrs := TStringList.Create;
  Primestrs.Sorted := True;
  Primestrs.Duplicates := DupIgnore;
  FillChar(Thing, Sizeof(Thing), 0);
  EntryString := '1234567';
  Permutate(1);
  FOR I := Pred(Primestrs.Count) DOWNTO 0 DO
    IF Is_prime[Strtoint(Primestrs[I])] THEN BEGIN
      Result := Primestrs[I];
      Primestrs.Free;
      Break;
    END;
END;

FUNCTION Problem042: Integer;
CONST C_FNAME =
    'C:\Users\bvbes\OneDrive\Documents\GitHub\ProjectEulerDelphi\problem_42.txt';

VAR Names: TStringList; Matrix: ARRAY [1 .. 6000] OF Boolean;
  X, Y, Count, Sum: Longint;
BEGIN
  Names := TStringList.Create;
  Names.Delimiter := Char(',');
  TRY Names.LoadFromFile(C_FNAME);
  EXCEPT
    ON E: EInOutError DO
        Writeln('File handling error occured. Reason: ', E.Message);
  END;
  FillChar(Matrix, Sizeof(Matrix), False);
  FOR X := 1 TO 100 DO Matrix[X * (X + 1) DIV 2] := True;
  Count := 0;
  Names.DelimitedText := Names.Text;
  FOR X := 0 TO Pred(Names.Count) DO BEGIN
    Sum := 0;
    FOR Y := 1 TO Length(Names[X]) DO Inc(Sum, Ord(Names[X][Y]) - 64);
    IF Matrix[Sum] THEN Inc(Count);
  END;
  Result := Count;
END;

FUNCTION Problem043: Uint64;
TYPE
  TThingRec = RECORD
    Ch: Char;
    Occ: Boolean;
  END;

VAR Thing: ARRAY [1 .. 255] OF TThingRec; EntryString: STRING; Sum: Uint64;

  FUNCTION Divisibility(Dummy: Uint64): Boolean;

  CONST P: ARRAY [1 .. 7] OF Uint32 = (17, 13, 11, 7, 5, 3, 2);

  VAR J: Longint;
  BEGIN
    Divisibility := True;
    FOR J := 1 TO 7 DO BEGIN
      IF ((Dummy MOD 1000) MOD P[J]) <> 0 THEN BEGIN
        Divisibility := False;
        Break;
      END
      ELSE Dummy := Dummy DIV 10;
    END;
  END;

  PROCEDURE Permutate(Num: Byte);

  VAR I: Byte; Number: Uint64;
  BEGIN
    IF Num > Length(EntryString) THEN BEGIN
      Number := 0;
      FOR I := 1 TO Length(EntryString) DO
          Number := Number * 10 + (Ord(Thing[I].Ch) - 48);
      IF Divisibility(Number) THEN Inc(Sum, Number);
    END ELSE BEGIN
      FOR I := 1 TO Length(EntryString) DO BEGIN
        IF (NOT Thing[I].Occ) THEN BEGIN
          Thing[I].Occ := True;
          Thing[I].Ch := EntryString[Num];
          Permutate(Succ(Num));
          Thing[I].Occ := False;
        END;
      END;
    END;
  END;

BEGIN
  Sum := 0;
  FillChar(Thing, Sizeof(Thing), 0);
  EntryString := '1234567890';
  Permutate(1);
  Result := Sum;
END;

FUNCTION Problem044: Integer;
VAR Pentagonals: ARRAY OF Integer; I, J, K, D: Integer;
BEGIN
  SetLength(Pentagonals, 5000);
  FOR I := 1 TO 5000 DO Pentagonals[I - 1] := (3 * I * I - I) DIV 2;
  Result := MaxInt;
  FOR I := 1 TO HIGH(Pentagonals) DO
    FOR J := I + 1 TO HIGH(Pentagonals) DO BEGIN
      K := Pentagonals[J] - Pentagonals[I];
      D := Pentagonals[J] + Pentagonals[I];
      IF (K < Pentagonals[HIGH(Pentagonals)]) AND
        (D < Pentagonals[HIGH(Pentagonals)]) THEN
        IF BinarySearch(Pentagonals, K) AND BinarySearch(Pentagonals, D) THEN
            Result := Min(Result, K);
    END;
END;

FUNCTION Problem045: Uint64;
VAR Top, Triangle, I: Uint64;

BEGIN
  Top := 0;
  I := 285;
  REPEAT
    Inc(I);
    Triangle := I * (I + 1) DIV 2;
    IF Triangle > Top THEN Top := Triangle
    ELSE Writeln(I, '   ', Triangle);
  UNTIL IsPentagonal(Triangle) AND IsHexagonal(Triangle);
  Result := Triangle;
END;

FUNCTION Problem046: Uint32;
CONST Max = 1000000;
  // can be big, we don't iterate through it so no time penalty

TYPE
  TIntegerList = Tlist<Uint32>;

VAR Primes: TIntegerList; Top, Number, Goldbach, I: Uint32;
  Prime, Found: Boolean; Is_prime: ARRAY [2 .. Max] OF Boolean;

BEGIN
  Result := 0;
  Primes := TIntegerList.Create;
  Primes.Add(2);
  Number := 3;
  WHILE Primes.Count < Max DO
  // max can be huge, we "halt" when done
  BEGIN
    // start finding primes
    Prime := True;
    Top := Trunc(Sqrt(Number));
    FOR I := 0 TO Primes.Count - 1 DO BEGIN
      IF Primes.Items[I] > Top THEN Break;
      Prime := Number MOD Primes.Items[I] <> 0;
      IF NOT Prime THEN Break;
    END;
    IF Prime THEN BEGIN
      Primes.Add(Number);
      Is_prime[Number] := True;
    END
    ELSE // if number is not a prime, start checking
    BEGIN
      Found := False;
      I := 1;
      Goldbach := 2 * (I * I);
      WHILE Goldbach < Number DO BEGIN
        IF Is_prime[Number - Goldbach] THEN BEGIN
          Found := True;
          Break;
        END;
        Inc(I);
        Goldbach := (2 * (I * I))
      END;
      IF NOT Found THEN BEGIN
        Primes.Free;
        Exit(Number);
      END;
    END;
    Inc(Number, 2);
  END;
END;

FUNCTION Problem047: Uint32;
TYPE
  TIntegerList = Tlist<Uint32>;

VAR List: TIntegerList;

  FUNCTION BinarySearch(X: Uint32; VAR Indx: Integer): Boolean;

  VAR Top, Bottom, Middle, Toosmall: Integer; Found: Boolean;
  BEGIN
    Top := Pred(List.Count);
    Bottom := 0;
    Found := False;
    Indx := -1;
    REPEAT
      Middle := (Top + Bottom) DIV 2;
      IF (List[Top] = X) OR (List[Middle] = X) OR (List[Bottom] = X) THEN BEGIN
        Found := True;
        IF List[Top] = X THEN Indx := Top
        ELSE IF List[Middle] = X THEN Indx := Middle
        ELSE Indx := Bottom;
      END;
      Toosmall := Top - Bottom;
      IF X > List[Middle] THEN Bottom := Middle
      ELSE Top := Middle;
    UNTIL (Found OR (Toosmall < 3));
    Result := Found;
    IF NOT Found THEN BEGIN
      Indx := Top;
      WHILE List[Indx] > X DO Dec(Indx);
      Inc(Indx);
    END;
  END;

  PROCEDURE AddToList(X: Uint32);

  VAR Ix: Integer;
  BEGIN
    IF List.Count = 0 THEN List.Add(X)
    ELSE IF X > List[Pred(List.Count)] THEN List.Add(X)
    ELSE IF X < List[0] THEN List.Insert(0, X)
    ELSE IF NOT BinarySearch(X, Ix) THEN List.Insert(Ix, X);
  END;

  FUNCTION Npf(Number: Integer): Integer;

  VAR I: Integer;

  BEGIN
    I := 2;
    List.Clear;
    WHILE (I < Sqrt(Number)) OR (Number = 1) DO BEGIN
      IF Number MOD I = 0 THEN BEGIN
        Number := Number DIV I;
        AddToList(I);
        Dec(I);
      END;
      Inc(I);
    END;
    Result := Succ(List.Count);
  END;

VAR J: Integer;

BEGIN
  List := TIntegerList.Create;
  J := 2 * 3 * 5 * 7;
  WHILE True DO BEGIN
    IF (Npf(J) = 4) AND (Npf(J + 1) = 4) AND (Npf(J + 2) = 4) AND
      (Npf(J + 3) = 4) THEN BEGIN
      List.Free;
      Exit(J);
    END;
    Inc(J);
  END;
END;

FUNCTION Problem048: STRING;
VAR Sum, Term, Modulus: BigInteger; I, J: Uint32;
BEGIN
  Modulus := 10000000000;
  Sum := 0;
  FOR I := 1 TO 1000 DO BEGIN
    Term := I;
    FOR J := 2 TO I DO Term := (Term * I) MOD Modulus;
    Sum := (Sum + Term) MOD Modulus;
  END;
  Result := STRING(Sum);
END;

FUNCTION Problem049: STRING;
TYPE
  TDynBools = TArray<Boolean>;

CONST Max_number = 10000;

VAR Is_prime: TDynBools; X, Addnumber: Integer;

BEGIN
  Is_prime := Sieve(Max_number);
  X := 1487;
  WHILE True DO BEGIN
    Inc(X, 2);
    Addnumber := 1;
    WHILE (X + 2 * Addnumber) < Max_number DO BEGIN
      IF Is_prime[X] AND Is_prime[X + Addnumber] AND Is_prime[X + 2 * Addnumber]
      THEN // check for primarity
        IF (Sort_number(X) = Sort_number(X + Addnumber)) AND
          (Sort_number(X) = Sort_number(X + 2 * Addnumber)) THEN
          // check for permutation
            Exit(Inttostr(X) + Inttostr(X + Addnumber) +
            Inttostr(X + 2 * Addnumber));
      Inc(Addnumber);
    END;
  END;
END;

FUNCTION Problem050: Uint32;
CONST Max = 1000000;

TYPE
  TIntegerList = Tlist<Uint32>;
  TDynBools = TArray<Boolean>;

VAR Primes: TIntegerList; I, J, Longest, Largest, Sum: Longint;
  Is_prime: TDynBools;

BEGIN
  Primes := TIntegerList.Create;
  Is_prime := Sieve(Max);
  FOR I := LOW(Is_prime) TO HIGH(Is_prime) DO
    IF Is_prime[I] THEN Primes.Add(I);

  Longest := 0;
  Largest := 0;
  FOR I := 0 TO Pred(Primes.Count) DO BEGIN
    Sum := 0;
    FOR J := I + 1 TO Pred(Primes.Count) DO BEGIN
      Inc(Sum, Primes[J]);
      IF Sum > Max THEN Break;
      IF (Is_prime[Sum]) AND (J - I > Longest) THEN BEGIN
        Longest := J - I;
        Largest := Sum;
      END;
    END;
  END;
  Result := Largest;
END;

END.
