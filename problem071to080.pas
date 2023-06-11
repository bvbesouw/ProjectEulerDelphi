unit problem071to080;

interface
USES Euler, Sysutils, Classes, Math, System.Generics.Collections,windows, Velthuis.bigintegers;
function problem071 : string;
function problem072 : string;
function problem073: uint32;
Function problem074 : integer;
Function problem075 : uint64;
function problem076 : uint64;
function problem077: String;
function problem078 : integer;
function problem079: string;
 function problem080: string;

implementation
function problem071 : string;
begin
  Result := 'not solved yet!!!!';
end;

function problem072 : string;
begin
  Result := 'not solved yet!!!!';
end;

function problem073: uint32;
VAR numerator, denominator,count : uint32;
BEGIN
  count := 0;
  FOR denominator := 2 TO 12000 DO
    FOR numerator := (denominator DIV 3) + 1 TO (denominator - 1) DIV 2 DO
      IF (gcd(numerator, denominator) = 1) THEN
        inc(count);
  result := count;
END;

Function problem074 : integer;
const facs : array[0..9] OF longint = (1,1,2, 6,24,120,720,5040,40320,362880);
VAR
  list: Tlist<integer>;

function GetSumOffacs(num: longint): longint;
begin
  result := 0;
  while num > 0 do
  begin
    Inc(result, facs[num mod 10]);
    num := num div 10;
  end;
end;

VAR i, sum, count : longint;
  already_exists : boolean;
BEGIN
  list := Tlist<integer>.Create;
  count := 0;
  FOR i := 1 TO 1000000 DO
    BEGIN
      sum := i;
      list.clear;
      list.add(sum);
      already_exists := false;
      REPEAT
        sum := GetSumOffacs(sum);
        IF list.IndexOf(sum) > -1 THEN already_exists := true
        ELSE
          list.add(sum);
      UNTIL already_exists or (list.Count > 60);
      IF list.count = 60 THEN inc(count);
    END;
  result := count;
END;

function problem075: uint64;
const
  limit = 1500000;
var
  mlimit, m, n, abc, p: longint;
  triangles: TList<longint>;
begin
  result := 0;
  triangles := TList<longint>.Create;
  try
    triangles.Count := limit + 1;
    mlimit := trunc(sqrt(limit / 2));
    for m := 2 to pred(mlimit) do
      for n := 1 to pred(m) do
        if (odd(n + m) and (gcd(n, m) = 1)) then
        begin
          abc := 2 * (m * m) + 2 * m * n;
          p := abc;
          while p < limit do
          begin
            triangles[p] := triangles[p] + 1;
            case triangles[p] of
              1: inc(result);
              2: dec(result);
            end;
            inc(p, abc);
          end;
        end;
  finally
    triangles.Free;
  end;
end;

function problem076 : uint64;
TYPE
  TIntegerList = tlist<longint>;

CONST n = 100;

VAR
  ways : array[0..10000] OF int64;
  numbers: Tintegerlist;
  i,j : longint;

BEGIN
  numbers := tintegerlist.create;
  FOR i := 1 TO n-1 DO
    numbers.add(i);
  ways[0] := 1;
  FOR i := 0 TO pred(numbers.count) DO
    FOR j := numbers[i] TO n DO
      ways[j] := ways[j] + ways[j-numbers[i]];
  result := ways[n];
END;

function problem077: string;

begin
  Result := 'not solved yet!!!!';
end;

function problem078 : integer;
const
  K = 100000;
  Target = 1000000;

type
  TInt64Array = array of Int64;

var
  Partitions: TInt64Array;
  n: Integer;

function CalculatePartitions(i: Integer): Int64;
var
  j, k, s: Integer;
begin
  s := 0;
  for j := 1 to i do
  begin
    k := j * (3 * j - 1) div 2;
    if k > i then break;
    if j mod 2 = 0 then s := s - Partitions[i - k] else s := s + Partitions[i - k];
    k := k + j;
    if k > i then continue;
    if j mod 2 = 0 then s := s - Partitions[i - k] else s := s + Partitions[i - k];
  end;
  Result := s mod Target;
end;

procedure ComputePartitions;
var
  i: Integer;
begin
  SetLength(Partitions, K + 1);
  Partitions[0] := 1;
  for i := 1 to K do
    Partitions[i] := CalculatePartitions(i);
end;

begin
  ComputePartitions;
  n := 0;
  while Partitions[n] mod Target <> 0 do
    Inc(n);
  result := n;
end;



function problem079:string;
begin
  Result := 'not solved yet!!!!';
 end;

function problem080: string;
begin
  Result := 'not solved yet!!!!';
 end;




end.
