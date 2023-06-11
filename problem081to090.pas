unit problem081to090;

interface
USES Euler, Sysutils, Classes, Math, System.Generics.Collections,windows, Velthuis.bigintegers;
function problem081 : string;
function problem082 : string;
function problem083 : string;
function problem084 : string;
function problem085 : string;
function problem086 : string;
function problem087: integer;

implementation
function problem081 : string;
begin
  Result := 'not solved yet!!!!';
end;
function problem082 : string;
begin
  Result := 'not solved yet!!!!';
end;
function problem083 : string;
begin
  Result := 'not solved yet!!!!';
end;
function problem084 : string;
begin
  Result := 'not solved yet!!!!';
end;
function problem085 : string;
begin
  Result := 'not solved yet!!!!';
end;
function problem086 : string;
begin
  Result := 'not solved yet!!!!';
end;

function problem087: integer;
TYPE
  TIntegerList = tlist<uint64>;
  Tboollist= tlist<boolean>;

VAR
  primes: Tintegerlist;
  i1,i2,i3,a,b,c,top, max, number, p: uint64;
  prime: boolean;
  q : uint64;
  arr : tboollist;

BEGIN
  max := 7072;
  primes := TIntegerList.Create;
  primes.Add(2);
  arr := tboollist.Create;
  for p := 1 to 50000000 do arr.Add(False);
  result := 0;
  number := 3;
  WHILE number <= max DO
    BEGIN
      prime := True;
      top := trunc(sqrt(number));
      FOR p := 0 TO primes.Count - 1 DO
        BEGIN
          IF primes.items[p] > top THEN break;
          prime := number MOD primes.Items[p] <> 0;
          IF NOT prime THEN break;
        END;
      IF prime THEN primes.add(number);
      Inc(number, 2);
    END;
  FOR p := 0 TO pred(primes.count) DO
    BEGIN
      IF primes[p] < 85 THEN c := p;
      IF primes[p] < 369 THEN b := p;
      IF primes[p] < 7072 THEN a := p;
    END;

  FOR i1 := 0 TO a DO
    FOR i2 := 0 TO b DO
      FOR i3 := 0 TO c DO
        BEGIN
          q := primes[i1]  * primes[i1] + primes[i2]*primes[i2]*primes[i2] + primes[i3]*primes[i3]*primes[i3]*primes[i3];
          IF q >= 50000000 THEN break
          ELSE
            IF NOT arr[q] THEN
              BEGIN
                inc(result);
                arr[q] := true;
              END;
        END;
END;


end.
