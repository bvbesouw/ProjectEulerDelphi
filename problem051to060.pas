UNIT problem051to060;

INTERFACE

USES Euler, Sysutils, Classes, Math, System.Generics.Collections,
  System.StrUtils, Velthuis.BigIntegers;
TYPE
  TDynBools = TArray<Boolean>;

FUNCTION Problem051: Uint32;
FUNCTION Problem052: Uint32;
FUNCTION Problem053: Integer;
Function Problem054: String;
Function Problem055: Integer;
Function Problem056: Integer;
Function Problem057: Integer;
Function Problem058: Uint32;
Function Problem059: String;
Function Problem060: Uint32;

IMPLEMENTATION

FUNCTION Problem051: Uint32;
CONST MAX = 999999;
  MyArray: TArray<STRING> = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
VAR Is_prime: TDynBools; C, Count: Uint32; Digit, P: Integer; Q: STRING;
BEGIN
  Result := 0;
  Is_prime := Sieve(MAX);
  FOR C := 100000 TO MAX DO BEGIN
    IF NOT Is_prime[C] THEN Continue;
    Count := 0;
    Digit := HasRepeatedDigit(C);
    IF Digit > 0 THEN BEGIN
      FOR Q IN MyArray DO BEGIN
        P := StrToInt(ReplaceStr(IntToStr(C), IntToStr(Digit), Q));
        IF (P > 100000) AND Is_prime[P] THEN BEGIN
          Inc(Count);
          IF Count = 8 THEN Exit(C);
        END;
      END;
    END;
  END;
END;

FUNCTION Problem052: Uint32;
VAR I: Uint32;

BEGIN
  I := 1;
  WHILE True DO
    IF (Sort_number(6 * I) = Sort_number(5 * I)) AND
      (Sort_number(5 * I) = Sort_number(4 * I)) AND
      (Sort_number(4 * I) = Sort_number(3 * I)) AND
      (Sort_number(3 * I) = Sort_number(2 * I)) THEN Exit(I)
    ELSE Inc(I);
END;

FUNCTION Problem053: Integer;

  FUNCTION Binomial(N, K: Integer): BigInteger;
  VAR I: Integer; Res: BigInteger;
  BEGIN
    IF (K < 0) OR (K > N) THEN Exit(0);
    IF K > N - K THEN K := N - K;
    Res := 1;
    FOR I := 1 TO K DO Res := Res * BigInteger(N - I + 1) DIV I;
    Result := Res;
  END;

CONST L = 1000000; Maxn = 100;
VAR C, N, R: Integer;
BEGIN
  C := 0;
  FOR N := 23 TO Maxn DO BEGIN
    FOR R := 2 TO N DIV 2 DO BEGIN
      IF Binomial(N, R) > L THEN BEGIN
        Inc(C, N - 2 * R + 1);
        Break;
      END;
    END;
  END;
  Result := C;
END;

Function Problem054: String;
Begin
  Result := 'not solved yet!!!!';
End;

Function Problem055: Integer;
Var P: Integer;
Begin
  Result := 0;
  For P := 1 To 10000 Do
    If IsLychrelNumber(P) Then Inc(Result);
End;

Function Problem056: Integer;
Var A, B, Count: Integer;
Begin
  Result := 0;
  For A := 1 To 99 Do
    For B := 1 To 99 Do Begin
      Count := GetSumOfDigits(Biginteger.Pow(A, B));
      If Count > Result Then Result := Count;
    End;
End;

Function Problem057: Integer;
VAR P, Q, P1, Q1, C: Biginteger; I, Counter: Integer;
BEGIN
  P := 1;
  Q := 1;
  Counter := 0;
  FOR I := 1 TO 1000 DO BEGIN
    C := Q * 2;
    P1 := P + C;
    Q1 := P + Q;
    IF Length(P1.ToString) > Length(Q1.ToString) THEN Inc(Counter);
    P := P1;
    Q := Q1;
  END;
  Result := Counter;
END;

Function Problem058: Uint32;
Const Percent = 10 / 100;
Var Prime_number, X, Y, Total_numbers, Prime_numbers: Uint32;
Begin
  Prime_numbers := 0;
  X := 3;
  While True Do Begin
    For Y := 1 To 3 Do
      If Isprime(X * X - Y * (X - 1)) Then Inc(Prime_numbers);

    Total_numbers := 2 * X - 1;
    If Prime_numbers / Total_numbers < Percent Then Exit(X);
    Inc(X, 2);
  End;
End;

Function Problem059: String;
Begin
  Result := 'not solved yet!!!!';
End;

Function Problem060: Uint32;
  FUNCTION CombineAndCheckPrime(A, B: Uint32): Boolean;
  BEGIN
    Result := IsPrime(A * Trunc(Power(10, Num_Of_Digits(B))) + B) AND
      IsPrime(B * Trunc(Power(10, Num_Of_Digits(A))) + A);
  END;

VAR Primes: TList<Integer>;
     A, B, C, D, E, Number, Top, P, Temp: Uint32;

  Prime: Boolean;

BEGIN
  Primes := TList<Integer>.Create;
  Primes.Add(2);
  Number := 3;
  WHILE Number < 10000 DO BEGIN
    Prime := True;
    Top := Trunc(Sqrt(Number));
    FOR P := 0 TO Primes.Count - 1 DO BEGIN
      IF Primes.Items[P] > Top THEN Break;
      Prime := Number MOD Primes.Items[P] <> 0;
      IF NOT Prime THEN Break;
    END;
    IF Prime THEN Primes.Add(Number);
    Inc(Number, 2);
  END;

  FOR A := 1 TO Pred(Primes.Count) DO BEGIN
    FOR B := Succ(A) TO Pred(Primes.Count) DO
      IF CombineAndCheckPrime(Primes[A], Primes[B]) THEN BEGIN
        FOR C := Succ(B) TO Pred(Primes.Count) DO
          IF (CombineAndCheckPrime(Primes[A], Primes[C]) AND
            CombineAndCheckPrime(Primes[B], Primes[C])) THEN BEGIN
            FOR D := Succ(C) TO Pred(Primes.Count) DO
              IF (CombineAndCheckPrime(Primes[A], Primes[D]) AND
                CombineAndCheckPrime(Primes[B], Primes[D]) AND
                CombineAndCheckPrime(Primes[C], Primes[D])) THEN BEGIN
                FOR E := Succ(D) TO Pred(Primes.Count) DO
                  IF (CombineAndCheckPrime(Primes[A], Primes[E]) AND
                    CombineAndCheckPrime(Primes[B], Primes[E]) AND
                    CombineAndCheckPrime(Primes[C], Primes[E]) AND
                    CombineAndCheckPrime(Primes[D], Primes[E])) THEN BEGIN
                    Temp := (Primes[A] + Primes[B] + Primes[C] + Primes[D] +
                      Primes[E]);
                    Primes.Free;
                    Exit(Temp);
                  END;
              END;
          END;
      END;
  END;
  Result := 0;
  Primes.Free;
END;

END.
