UNIT problem001to010;

INTERFACE

USES Euler, System.Math;
FUNCTION Problem001: Integer;
FUNCTION Problem002: Integer;
FUNCTION Problem003: Integer;
FUNCTION Problem004: Integer;
FUNCTION Problem005: Integer;
FUNCTION Problem006: Integer;
FUNCTION Problem007: Integer;
FUNCTION Problem008: Int64;
FUNCTION Problem009: Integer;
FUNCTION Problem010: Uint64;

TYPE
        TDynBools = TArray<Boolean>;

IMPLEMENTATION

FUNCTION Problem001: Integer;
VAR X: Integer;
BEGIN
        Result := 0;
        FOR X := 1 TO 999 DIV 3 DO Inc(Result, X * 3);
        FOR X := 1 TO 999 DIV 5 DO Inc(Result, X * 5);
        FOR X := 1 TO 999 DIV 15 DO Dec(Result, X * 15);
END;

FUNCTION Problem002: Integer;
VAR X, Y, Total: Longint;
BEGIN
        X := 1;
        Y := 1;
        Result := 0;
        Total := X + Y;
        REPEAT
                IF NOT Odd(Total) THEN Inc(Result, Total);
                X := Y;
                Y := Total;
                Total := X + Y;
        UNTIL Total > 4000000;
END;

FUNCTION Problem003: Integer;

CONST Max_number = 548112;
        // sqrt(600851475143 div 2);

VAR Is_prime: TDynBools; Lin, Lcnt: Uint64;

BEGIN
        Lin := 600851475143;
        Lcnt := 2;
        Result := 0;
        Is_prime := Sieve(Max_number);
        WHILE Lcnt * Lcnt <= Lin DO BEGIN
                IF Lin MOD Lcnt = 0 THEN
                        IF IsPrime(Lcnt) AND (Lcnt > Result) THEN
                                  Result := Lcnt;
                Lcnt := Lcnt + 1;
        END;

END;

FUNCTION Problem004: Integer;
CONST Max = 999;

VAR X, Y, Mul: Longint;

BEGIN
        Result := 0;
        X := Max;
        WHILE X > 100 DO BEGIN
                FOR Y := Max DOWNTO 100 DO BEGIN
                        Mul := X * Y;
                        IF ((Mul > Result) AND IsPalindrome(Mul)) THEN
                                  Result := Mul;
                END;
                IF ((X < Max) AND (Result = Mul)) THEN Break;
                Dec(X);
        END;
END;

FUNCTION Problem005: Integer;
CONST Top = 20;
        P: ARRAY [1 .. 10] OF Integer = (2, 3, 5, 7, 11, 13, 17, 19, 23, 29);

VAR I: Integer; Log_n: Real;

BEGIN
        I := 1;
        Result := 1;
        Log_n := Log10(Top);
        WHILE P[I] <= Sqrt(Top) DO BEGIN
                Result := Result *
                  Trunc(Intpower(P[I], Trunc(Log_n / Log10(P[I]))));
                Inc(I);
        END;
        WHILE P[I] <= Top DO BEGIN
                Result := Result * P[I];
                Inc(I);
        END;
END;

FUNCTION Problem006: Integer;
VAR N, Sum1, Sum2: Integer;
BEGIN
        Sum1 := 0;
        Sum2 := 0;
        FOR N := 1 TO 100 DO BEGIN
                Sum1 := Sum1 + (N * N);
                Sum2 := Sum2 + N;
        END;
        Sum2 := Sum2 * Sum2;
        Result := Abs(Sum1 - Sum2);
END;

FUNCTION Problem007: Integer;
CONST N = 10001;
VAR Primes: ARRAY [0 .. N - 1] OF Integer; Count, I, J: Integer;
        IsPrime: Boolean; Sqrti: Integer;
BEGIN
        Primes[0] := 2;
        Count := 1;
        I := 3;
        WHILE Count < N DO BEGIN
                IsPrime := True;
                Sqrti := Round(Sqrt(I));
                FOR J := 0 TO Count - 1 DO BEGIN
                        IF Primes[J] > Sqrti THEN Break;
                        IF I MOD Primes[J] = 0 THEN BEGIN
                                IsPrime := False;
                                Break;
                        END;
                END;
                IF IsPrime THEN BEGIN
                        Primes[Count] := I;
                        Inc(Count);
                END;
                Inc(I, 2);
        END;
        Result := Primes[N - 1];
END;

FUNCTION Problem008: Int64;
CONST Number =
          '73167176531330624919225119674426574742355349194934969835203127745063262395783180169848018694788518'
          + '43858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304'
          + '35576689664895044524452316173185640309871112172238311362229893423380308135336276614282806444486645'
          + '23874930358907296290491560440772390713810515859307960866701724271218839987979087922749219016997208'
          + '88093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441'
          + '57221553975369781797784617406495514929086256932197846862248283972241375657056057490261407972968652'
          + '41453510047482166370484403199890008895243450658541227588666881164271714799244429282308634656748139'
          + '19123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421'
          + '75069416589604080719840385096245544436298123098787992724428490918884580156166097919133875499200524'
          + '06368991256071760605886116467109405077541002256983155200055935729725716362695618826704282524836008'
          + '23257530420752963450'; Lngth = 13;

VAR Product: Uint64; I, J: Integer;

BEGIN
        Result := 0;
        FOR I := 1 TO Length(Number) - Lngth DO BEGIN
                Product := 1;
                FOR J := 0 TO Pred(Lngth) DO
                          Product := Product * (Ord(Number[I + J]) - 48);
                IF Product > Result THEN Result := Product;
        END;
END;

FUNCTION Problem009: Integer;

CONST Total = 1000;

VAR A, B, C: Integer;

BEGIN
        Result := -1;
        FOR A := 3 TO Total DO
                FOR B := 1 TO A - 1 DO BEGIN
                        C := Total - (A + B);
                        IF (A * A + B * B = C * C) THEN BEGIN
                                Result := A * B * C;
                                Exit
                        END;
                END;
END;

FUNCTION Problem010: Uint64;
CONST Max_number = 2000000;

VAR Is_prime: TDynBools; I: Longint;

BEGIN
        Is_prime := Sieve(Max_number);
        Result := 0;
        FOR I := 1 TO Max_number DO
                IF Is_prime[I] THEN Inc(Result, I);
END;

END.
