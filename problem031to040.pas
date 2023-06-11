UNIT problem031to040;

INTERFACE

USES Classes, Sysutils, Math, Euler, Generics.Collections;
FUNCTION Problem031: Integer;
FUNCTION Problem032: Integer;
FUNCTION Problem033: Integer;
FUNCTION Problem034: Integer;
FUNCTION Problem035: Integer;
FUNCTION Problem036: Integer;
FUNCTION Problem037: Integer;
FUNCTION Problem038: Integer;
FUNCTION Problem039: Integer;
FUNCTION Problem040: Integer;

IMPLEMENTATION

FUNCTION Problem031: Integer;

CONST Coins: ARRAY [0 .. 7] OF Integer = (1, 2, 5, 10, 20, 50, 100, 200);
VAR Ways: ARRAY [0 .. 200] OF Integer; I, J: Integer;
BEGIN
  FillChar(Ways, SizeOf(Ways), 0);
  Ways[0] := 1;
  FOR I IN Coins DO
    FOR J := I TO 200 DO Ways[J] := Ways[J] + Ways[J - I];
  Result := Ways[200];
END;

FUNCTION Problem032: Integer;
{ maybe rewrite without using strings }
VAR Products, I, J: Integer; S: STRING; List: Tstringlist;

BEGIN
  List := Tstringlist.Create;
  List.Sorted := True;
  List.Duplicates := DupIgnore;
  Products := 0;
  FOR I := 1 TO 8 DO
    FOR J := 999 TO 9999 DO BEGIN
      S := Inttostr(I) + Inttostr(J) + Inttostr(I * J);
      IF IsPandigital(S) THEN List.Add(Inttostr(I * J))
      ELSE IF Length(S) > 9 THEN Break;
    END;

  // for double digit multiplicand
  FOR I := 9 TO 99 DO
    FOR J := 99 TO 999 DO BEGIN
      S := Inttostr(I) + Inttostr(J) + Inttostr(I * J);
      IF IsPandigital(S) THEN List.Add(Inttostr(I * J))
      ELSE IF Length(S) > 9 THEN Break;
    END;
  FOR I := 0 TO Pred(List.Count) DO Inc(Products, Strtoint(List[I]));
  Result := Products;
END;

FUNCTION Problem033: Integer;
VAR Denproduct, Nomproduct, I, Nom, Den: Longint;
BEGIN
  Denproduct := 1;
  Nomproduct := 1;
  FOR I := 1 TO 9 DO
    FOR Den := 1 TO Pred(I) DO
      FOR Nom := 1 TO Pred(Den) DO
        IF ((Nom * 10 + I) * Den = Nom * (I * 10 + Den)) THEN BEGIN
          Denproduct := Denproduct * Den;
          Nomproduct := Nomproduct * Nom;
        END;
  Denproduct := Denproduct DIV Gcd(Nomproduct, Denproduct);
  Result := Denproduct;
END;

FUNCTION Problem034: Integer;
CONST Upperlimit = 999999;
VAR Arr: ARRAY [0 .. 9] OF Longint; I, A, Sumoffacs, Digit: Longint;

BEGIN
  Result := 0;
  Arr[0] := 1;
  // by definition 0! = 1
  FOR I := 1 TO 9 DO Arr[I] := Arr[I - 1] * I;
  FOR I := 10 TO Upperlimit DO BEGIN
    Sumoffacs := 0;
    A := I;
    REPEAT
      Digit := A MOD 10;
      Sumoffacs := Sumoffacs + Arr[Digit];
      A := A DIV 10;
    UNTIL A = 0;
    IF I = Sumoffacs THEN Inc(Result, I);
  END;
END;

FUNCTION Problem035: Integer;
VAR Primes: TArray<Boolean>; Pr: Integer;
  FUNCTION Is_circular(X: Longint): Boolean;

  { we can possibly optimize by keeping track of all numbers that have been checked and
    proven not to be circular, but this seems fast enough }

  VAR Y, I, P, Z: Longint;
  BEGIN
    IF (X < 17) THEN Exit(True);
    Result := True;
    Y := Num_Of_Digits(X);
    P := Intpower(10, Y - 1);
    FOR I := 1 TO Pred(Y) DO BEGIN
      Z := X;
      X := (Z DIV 10) + Z MOD 10 * P;
      IF NOT Primes[X] THEN BEGIN
        Result := False;
        Break;
      END;
    END;
  END;

BEGIN
  Result := 0;
  Primes := Sieve(1000000);
  FOR Pr := 2 TO 1000000 DO
    IF Primes[Pr] AND Is_circular(Pr) THEN Inc(Result);
END;

FUNCTION Problem036: Integer;
VAR P: Integer;
BEGIN
  Result := 0;
  FOR P := 1 TO 1000000 DO
    IF Ispalindrome(P) AND Ispalindrome(IntToBin(P)) THEN Inc(Result, P);
END;

FUNCTION Problem037: Integer;
TYPE
  TIntegerList = Tlist<Uint32>;

CONST Step = 250000;
  Dividers: ARRAY [2 .. 6] OF Uint32 = (10, 100, 1000, 10000, 100000);

VAR Primes: TIntegerList; Number, P, Sum, Foundnumbers: Uint32; Prime: Boolean;
  Arr: ARRAY OF Boolean;

  FUNCTION Right_Truncable(Numb: Uint32): Boolean;
  BEGIN
    Result := True;
    Numb := Numb DIV 10;
    REPEAT
      IF NOT Arr[Numb] THEN Exit(False);
      Numb := Numb DIV 10;
    UNTIL Numb = 0;
  END;

  FUNCTION Left_truncable(Numb: Uint32): Boolean;

  VAR X, I: Integer;
  BEGIN
    X := Num_Of_Digits(Numb);
    Result := True;
    FOR I := X DOWNTO 2 DO BEGIN
      Numb := Numb MOD Dividers[I];
      IF NOT Arr[Numb] THEN BEGIN;
        Result := False;
        Break
      END;
    END;
  END;

BEGIN
  Sum := 0;
  Foundnumbers := 0;
  Primes := TIntegerList.Create;
  Setlength(Arr, Step);
  Primes.Add(2);
  Arr[2] := True;
  Number := 3;
  WHILE Foundnumbers < 11 DO BEGIN
    Prime := True;
    FOR P := 0 TO Primes.Count - 1 DO BEGIN
      IF Primes.Items[P] > Trunc(Sqrt(Number)) THEN Break;
      Prime := Number MOD Primes.Items[P] <> 0;
      IF NOT Prime THEN Break;
    END;
    IF Prime THEN BEGIN
      IF Number > Pred(Length(Arr)) THEN Setlength(Arr, Length(Arr) + Step);
      Arr[Number] := True;
      Primes.Add(Number);
      IF Left_truncable(Number) AND Right_Truncable(Number) THEN BEGIN
        Inc(Sum, Number);
        Inc(Foundnumbers);
      END;
    END;
    Inc(Number, 2);
  END;
  Result := Sum;
END;

FUNCTION Problem038: Integer;
VAR X, Res: Integer;

BEGIN
  Result := 0;
  FOR X := 10000 DOWNTO 1 DO BEGIN
    Res := 100002 * X;
    IF (IsPandigital(Inttostr(Res))) THEN Exit(Res);
  END;
END;

FUNCTION Problem039: Integer;
VAR A, B, C, P, Max_solutions, Max_p: Integer; Solutions: Integer;
BEGIN
  Max_solutions := 0;
  Max_p := 0;
  FOR P := 3 TO 1000 DO BEGIN
    Solutions := 0;
    FOR A := 1 TO P DIV 3 DO BEGIN
      FOR B := A TO (P - A) DIV 2 DO BEGIN
        C := P - A - B;
        IF A * A + B * B = C * C THEN Inc(Solutions);
      END;
    END;
    IF Solutions > Max_solutions THEN BEGIN
      Max_solutions := Solutions;
      Max_p := P;
    END;
  END;
  Result := Max_p;
END;

FUNCTION Problem040: Integer;
VAR Str: STRING; I, N, Index, Product: Integer;
BEGIN
  Str := '';
  I := 1;
  WHILE Length(Str) < 1000000 DO BEGIN
    Str := Str + Inttostr(I);
    Inc(I);
  END;

  Product := 1;
  INDEX := 1;
  FOR N := 1 TO 6 DO BEGIN
    Product := Product * Strtoint(Str[INDEX]);
    INDEX := INDEX * 10;
  END;

  Result := Product;
END;

END.
