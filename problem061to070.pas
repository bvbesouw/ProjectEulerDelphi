unit problem061to070;

interface
USES Euler, Sysutils, Classes, Math, System.Generics.Collections,  Velthuis.BigIntegers;
function problem061 : uint64;
function problem062 : uint64;
function Problem063: Integer;
Function problem064: uint32;
function problem065: uint32;
function Problem066: string;
function problem067:integer;
function Problem068: string;
function problem069: uint32;
function problem070 : string;
type
  TNode = array[1..5] of Integer;

implementation
function problem061 : uint64;
var p : integer;
begin

  result := 1;
end;

function problem062 : uint64;
const
  N = 5;
var
  i: Int64;
  Cube: Int64;
  Digits: array[0..9] of Integer;
  j: Integer;
  Key: string;
  Cubes: TDictionary<string, TList<Int64>>;
  List: TList<Int64>;
begin
  Cubes := TDictionary<string, TList<Int64>>.Create;
  try
    i := 0;
    while True do
    begin
      Inc(i);
      Cube := i * i * i;
      FillChar(Digits, SizeOf(Digits), 0);
      while Cube > 0 do
      begin
        Inc(Digits[Cube mod 10]);
        Cube := Cube div 10;
      end;
      Key := '';
      for j := High(Digits) downto Low(Digits) do
        Key := Key + Digits[j].ToString + ',';
      if not Cubes.TryGetValue(Key, List) then
      begin
        List := TList<Int64>.Create;
        Cubes.Add(Key, List);
      end;
      List.Add(i * i * i);
      if List.Count = N then
        Exit(List[0]);
    end;
  finally
    for List in Cubes.Values do
      List.Free;
    Cubes.Free;
  end;
end;

function Problem063: Integer;
VAR i,counter,power : integer;
  dummy: biginteger;

BEGIN
  counter := 0;
  FOR i := 1 TO 10 DO
    BEGIN
      power := 1;
      WHILE true DO
        BEGIN
          dummy :=  BigInteger.Pow(i,power);
          IF power = length(dummy.ToString) THEN
            inc(counter)
          ELSE
            break;
          inc(power);
        END;
    END;
  result := counter;
END;

Function problem064: uint32;
FUNCTION cf(n : uint32) : uint32;

VAR period, a0,an : uint32;
  mn, dn : real;

BEGIN
  a0 := trunc(sqrt(n));
  an := a0;
  mn := 0;
  dn := 1;
  period := 0;
  IF a0 <> sqrt(n) THEN
    WHILE an <> (a0 + a0) DO
      BEGIN
        mn := dn*an-mn;
        dn := (n - mn*mn)/dn;
        an := trunc((a0 + mn)/dn);
        inc(period)
      END;
  cf := period;
END;


VAR
  counter, i : longint;
BEGIN
  counter := 0;
  FOR i := 1 TO 10000 DO
    IF odd(cf(i)) THEN inc(counter);
  result := counter;
END;

function problem065 : uint32;
CONST MAX = 100;

VAR fract,sum,k : Longword;
  n,prev_n,temp: biginteger;

BEGIN
  n := 2;
  temp:=1;

  FOR k := 2 TO MAX DO
    BEGIN
      IF k MOD 3 = 0 THEN
        BEGIN
          fract := k DIV 3;
          Inc(fract,fract);
        END
      ELSE
        fract := 1;
      prev_n := n;
      n := prev_n * fract + temp;
      temp := prev_n;
    END;
  sum := GetSumOfDigits(n.ToString);
  result := sum;
END;

function problem066 : string;
begin
  Result := 'not solved yet!!!!';
end;

function problem067:integer;
CONST
  C_FNAME = 'C:\Users\bvbes\OneDrive\Documents\GitHub\ProjectEulerDelphi\problem_67.txt';
  lines = 100;

VAR
  slInfo: TStringList;
  arr : ARRAY[1..lines,1..lines] OF integer;
  i,x,y : longint;
BEGIN
  slInfo := TStringList.Create;
  slinfo.delimiter := char(' ');
  TRY
    slInfo.LoadFromFile(C_FNAME);
  EXCEPT
    on E: EInOutError DO
          writeln('File handling error occured. Reason: ', E.Message);
  END;
  slinfo.delimitedText := slinfo.Text;
  //populate matrix
  i := 0;
  FOR x := 1 TO lines DO
    FOR y:= 1 TO x DO
      BEGIN
        arr[x,y] := strtoint(slinfo[i]);
        inc(i);
      END;
  //count from the bottom up
  FOR x := lines-1 DOWNTO 1 DO
    FOR y:= 1 TO x DO
      arr[x,y] := arr[x,y] + max(arr[x+1,y],arr[x+1,y+1]);
    result := arr[1,1];
  slInfo.Free;
END;

function problem068 : string;
begin
  Result := 'not solved yet!!!!';
end;

function problem069: uint32;
VAR
  is_prime: TArray<Boolean>;

VAR i, number : uint32;

BEGIN
  is_prime := Sieve(1000);
  number := 1;
  i := 2;
  while number * i <1000000 do
  begin
    number := number * i;
    inc(i);
    WHILE
      NOT is_prime[i] DO  inc(i);
  end;
  result := number;
END;

function problem070 : string;
begin
  Result := 'not solved yet!!!!';
end;
end.
