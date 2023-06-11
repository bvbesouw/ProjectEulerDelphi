
UNIT fractions;
























{ A unit for doing arithmatics with fractions

  Copyright (C) 2015 by Lazarus and FreePascal community
  (http://www.lazarus.freepascal.org and http://www.freepascal.org)
  Original code by
    Bart Broersma (www.flyingsheep.nl)
    HappyLarry

  Portions copyright by David Peterson and The Math Forum @ Drexel,
  (redistributed with the consent of the copyright holder)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$mode objfpc}{$H+}
{$MODESWITCH  ADVANCEDRECORDS}

INTERFACE

USES 
SysUtils, Math;

TYPE 

  { TFraction }

  TFraction = RECORD
    PRIVATE 
      FNumerator: Int64;
      FDenominator: Int64;
      PROCEDURE SetDominator(AValue: Int64);
    PUBLIC 
      PROCEDURE Normalize;
      FUNCTION ToString: String;
      FUNCTION Resolve: String;
      FUNCTION ToFloat: Double;
      property Numerator: Int64 read FNumerator write FNumerator;
      property Denominator: Int64 read FDenominator write SetDominator;
  END;

  TFloatToFractionFunc = FUNCTION (Value, Precision: Double): TFraction;
  TGreatestCommonDivisorFunc = FUNCTION (a, b: Int64): Int64;

FUNCTION Fraction(ANumerator, ADenominator: Int64): TFraction;
FUNCTION Fraction(AIntPart, ANumerator, ADenominator: Int64): TFraction;

operator = (F1: TFraction; F2: TFraction) B: Boolean;
operator < (F1: TFraction; F2: TFraction) B: Boolean;
operator > (F1: TFraction; F2: TFraction) B: Boolean;
operator <= (F1: TFraction; F2: TFraction) B: Boolean;
operator >= (F1: TFraction; F2: TFraction) B: Boolean;
operator := (I: Int64) F: TFraction;
operator := (S: STRING) F: TFraction;
operator + (L: TFraction; R: TFraction) F: TFraction;
operator - (L: TFraction; R: TFraction) F: TFraction;
operator - (L: TFraction) F: TFraction;
operator * (L: TFraction; R: TFraction) F: TFraction;
operator * (L: TFraction; R: Int64) F: TFraction;
operator * (L: Int64; R: TFraction) F: TFraction;
operator / (L: TFraction; R: TFraction) F: TFraction;
operator / (L: TFraction; R: Int64) F: TFraction;
operator ** (L: TFraction; R: Integer) F: TFraction;

FUNCTION GreatestCommonDivisor_DivisionBased(a, b: Int64): Int64;
FUNCTION GreatestCommonDivisor_EuclidSubraction(a, b: Int64): Int64;
FUNCTION CF_FloatToFraction(Value, Precision: Double): TFraction;
//based upon Continue Fractios
FUNCTION MF_FloatToFraction(Value, Precision: Double): TFraction;
//Original code by David Peterson and The Math Forum @ Drexel

FUNCTION StrToFraction(CONST S: STRING): TFraction;
FUNCTION TryStrToFraction(CONST S: STRING; out F: TFraction): Boolean;
FUNCTION StrToFractionDef(CONST S: STRING; Def: TFraction): TFraction;
FUNCTION TryFloatToFraction(Value, Precision: Double; out F: TFraction; AcceptPrecisionError: Boolean = True): Boolean;
FUNCTION FloatToFractionDef(Value, Precision: Double; Def: TFraction; AcceptPrecisionError: Boolean = True): TFraction;

FUNCTION Min(a, b: TFraction): TFraction;
inline;
overload;
FUNCTION Max(a, b: TFraction): TFraction;
inline;
overload;
FUNCTION InRange(CONST AValue, AMin, AMax: TFraction): Boolean;
inline;
overload;
FUNCTION EnsureRange(CONST AValue, AMin, AMax: TFraction): TFraction;
inline;
overload;
FUNCTION Sign(CONST AValue: TFraction): TValueSign;
inline;
overload;
FUNCTION IsZero(CONST AValue: TFraction): Boolean;
overload;
FUNCTION Abs(CONST AValue: TFraction): TFraction;
overload;

FUNCTION Floor(D: Double): Int64;
overload;
PROCEDURE AdjustPrecision(VAR Precision: Double; Value: Double);

VAR 
  FloatToFraction: TFloatToFractionFunc = @MF_FloatTofraction;
  GreatestCommonDivisor: TGreatestCommonDivisorFunc = @GreatestCommonDivisor_DivisionBased;

  ResourceString
  SDenominatorCannotBeZero = 'The denominator of a fraction cannot be 0';

CONST 
  FracSymbol: string = '/';
  AllowZeroPowerZero: Boolean = False;
  SDivisionByZero = 'Division by zero';
  SZeroPowerZero = 'Raising 0 to the power 0 is undefined';
  SInvalidFraction = '"%s" is not a valid fraction';
  SRangeCheckError = 'Range check error';

CONST 
  MaxInt64 = High(Int64);
  MinInt64 = Low(Int64);

IMPLEMENTATION

//Math.Floor returns Integers and does NOT cause RangeCheck or OverFlow errors,
//So Math.Floor(1/9E-11) returns
//  -1773790777 [FFFFFFFF964619C7] instead of
//  11111111111 [00000002964619C7]  which is the correct Int64 answer
//this causes an infinite loop in CF_FloatToFraction function

FUNCTION Floor(D: Double): Int64;
BEGIN
  Result := Trunc(D);
  IF Frac(D) < 0 THEN
    Result := Result - 1;
END;

FUNCTION GreatestCommonDivisor_DivisionBased(a, b: Int64): Int64;

VAR 
  temp: Int64;
BEGIN
  WHILE b <> 0 DO
    BEGIN
      temp := b;
      b := a MOD b;
      a := temp
    END;
  result := a
END;

FUNCTION GreatestCommonDivisor_EuclidSubraction(a, b: Int64): Int64;
//Euclids subtraction method (http://en.wikipedia.org/wiki/Euclidean_algorithm)
//only workswith positive integers
BEGIN
  IF (a = 0) THEN Exit(b);
  IF (b = 0) THEN Exit(a);
  IF (a < 0) THEN a := -a;
  IF (b < 0) THEN b := -b;
  WHILE NOT (a = b) DO
    BEGIN
      IF (a > b) THEN
        a := a - b
      ELSE
        b := b - a;
    END;
  Result := a;
END;


PROCEDURE AdjustPrecision(VAR Precision: Double; Value: Double);

CONST 
  MaxPrec = Double(1.0)/MaxInt64;
BEGIN
  Precision := Abs(Precision);
  IF ((Abs(Value) / Precision) > 1E15) THEN
    BEGIN
      //writeln('Value / Precision > 1E15');
      Precision := Abs(Value) / 1E16;
    END;
  IF (Precision < MaxPrec) THEN
    Precision := MaxPrec;
END;

FUNCTION InRange64(Value: Double): Boolean;
inline;
BEGIN
  Result := NOT ((Value > MaxInt64) OR (Value < MinInt64));
END;

PROCEDURE CheckRange(Value: Double);
BEGIN
  IF NOT InRange64(Value) THEN
    raise ERangeError.Create(SRangeCheckError);
END;

FUNCTION IsBorderlineValue(Value: Double; out F: TFraction): Boolean;

CONST 
  MaxPrec = Double(1.0)/MaxInt64;
  ZeroBoundary = MaxPrec / 2;
BEGIN
  IF (Abs(Value) <= MaxPrec) THEN
    BEGIN
      Result := True;
      //writeln('Abs(Value) < 1/MaxInt64 [',MaxPrec,']');
      IF (Abs(Value) < ZeroBoundary) THEN
        BEGIN
          //writeln('Abs(Value) < ZeroBoundary [',ZeroBoundary,']');
          F.Numerator := 0;
          F.Denominator := 1;
        END
      ELSE
        BEGIN
          IF (Value < 0) THEN
            F.Numerator := -1
          ELSE
            F.Numerator := 1;
          F.Denominator := MaxInt64;
        END;
    END
  ELSE
    Result := False;
END;

//uses method of Continued fractions
FUNCTION CF_FloatToFraction(Value, Precision: Double): TFraction;

VAR 
  H1, H2, K1, K2, A, NewA, tmp: Int64;
  B, diff, test: Double;
  PendingOverFlow, Found: Boolean;
BEGIN
  CheckRange(Value);
  AdjustPrecision(Precision, Value);
  //Borderline cases
  IF IsBorderlineValue(Value, Result) THEN
    Exit;
  H1 := 1;
  H2 := 0;
  K1 := 0;
  K2 := 1;
  b := Value;
  NewA := Round(Floor(b));
  REPEAT
    A := NewA;
    tmp := H1;
    H1 := (a * H1) + H2;
    H2 := tmp;
    tmp := K1;
    K1 := (a * K1) + K2;
    K2 := tmp;
    //write('H1=',H1,' K1=',K1,' A=',A);
    test := H1 / K1;
    //write(' test=',test);
    diff := Abs(test - Value);
    //write(' diff=',diff);
    Found := (diff < Precision);
    IF NOT Found THEN
      BEGIN
        IF (Abs(B-A) < 1E-30) THEN
          B := 1E30   //happens when H1/K2 exactly matches Value
        ELSE
          B := 1 / (B - A);
        //write(' B=',B);
        PendingOverFlow := (((Double(B) * H1) + H2) > MaxInt64) OR
                           (((Double(B) * K1) + K2) > MaxInt64) OR
                           (B > MaxInt64);
        //writeln(' PendingOverFlow=',PendingOverFlow);
        IF NOT PendingOverFlow THEN
          NewA := Round(Floor(B));
























{
      if PendingOverFlow then
      begin
        writeln('PendingOverFlow');
        writeln('New H1   = ',(Double(NewA) * H1) + H2);
        writeln('New K1   = ',(Double(NewA) * K1) + K2);
        writeln('B        = ',B);
        writeln('MaxInt64 = ',Double(MaxInt64));
      end;
      }
      END;
  UNTIL Found OR PendingOverFlow;
  Result.Numerator := H1;
  Result.Denominator := K1;
END;
























{
  This implementation of FloatToFraction was originally written by:
  David Peterson and The Math Forum @ Drexel
  Source: http://mathforum.org/library/drmath/view/51886.html
  It was ported to FreePascal by Bart Broersma
  Adjustments made:
    * Precision is bound by a magnitude of -15 to Value
    * Bordeline cases close to zero are handled
    * Handle negative values
    * Handle negative precision
    * Original code dealt with 32-bit integers, it was adjusted for 64-bit integers

  The original copyrigh holder has granted me permission to adjust and redistribute
  this code under modified LGPL license with linking exception (see COPYING.modifiedLGPL.txt).

  When redistributing this code, the comments above MUST also be redistributed with it!
}
FUNCTION MF_FloatToFraction(Value, Precision: Double): TFraction;

VAR 
  IntPart, Count, Num, Denom: Int64;
  i: Integer;
  TestLow, TestHigh, Test: Double;
  L,H: TFraction;
  IsNeg: Boolean;
BEGIN
  // find nearest fraction
  //writeln('MF_FloatToFraction:');
  //writeln('  Value = ',Value);
  //writeln('  Prec  = ',Precision);
  CheckRange(Value);
  AdjustPrecision(Precision, Value);
  //Borderline cases
  IF IsBorderlineValue(Value, Result) THEN
    Exit;
  IsNeg := (Value < 0);
  Value := Abs(Value);
  intPart := Round(Int(Value));
  Value := Frac(Value);
  L.Numerator := 0;
  L.Denominator := 1;
  H.Numerator := 1;
  H.denominator := 1;
  FOR i := 1 TO 100 DO
    //was 100
    BEGIN
      //writeln('  i = ',i);
      testLow := L.Denominator * Value - L.Numerator;
      testHigh := H.Numerator - H.Denominator * Value;
      //if (testHigh < Precision * H.Denominator) then
      IF (Abs(H.ToFloat - Value) < Precision) THEN
        BEGIN
          //writeln('  (testHigh < Precision * H.Denominator)');
          //writeln('  testHigh = ',testHigh);
          //writeln('  Precision * H.Denominator = ',Precision * H.Denominator);
          break;
          // high is answer
        END;
      //if (testLow < Precision * L.Denominator) then
      IF (Abs(L.ToFloat - Value) < Precision) THEN
        BEGIN
          // low is answer
          //writeln('  (testLow < Precision * L.Denominator)');
          //writeln('  testLow = ',testLow);
          //writeln('  Precision * L.Denominator = ',Precision * L.Denominator);
          H := L;
          break;
        END;
      IF Odd(i) THEN
        BEGIN
          // odd step: add multiple of low to high
          //writeln('  Odd step');
          test := testHigh / testLow;
          count := Round(Int(test));
          // "N"
          num := (count + 1) * L.Numerator + H.Numerator;
          denom := (count + 1) * L.Denominator + H.Denominator;
          IF ((num > High(Int64) - 1) OR    // was 8000, 10000
             (denom > High(Int64) - 1)) THEN
            //if ((num > $8000) or    // was 8000, 10000
            //(denom > $10000)) then
            BEGIN
              //writeln('  ((num > High(Int64) - 1) or (denom > High(Int64) - 1))');
              break;
            END;
          H.Numerator := num - L.Numerator;
          // new "A"
          H.Denominator := denom - L.Denominator;
          L.Numerator := num;
          // new "B"
          L.Denominator := denom;
        END
      ELSE
        BEGIN
          // even step: add multiple of high to low
          //writeln('  Even step');
          test := testLow / testHigh;
          count := Round(Int(test));
          // "N"
          num := L.Numerator + (count + 1) * H.Numerator;
          denom := L.Denominator + (count + 1) * H.Denominator;
          IF ((num > High(Int64) - 1) OR   //10000. 10000
             (denom > High(Int64) - 1)) THEN
            //if ((num > $10000) or   //10000. 10000
            //(denom > $10000)) then
            BEGIN
              //writeln('  ((num > High(Int64) - 1) or (denom > High(Int64) - 1))');
              break;
            END;
          L.Numerator := num - H.Numerator;
          // new "A"
          L.Denominator := denom - H.Denominator;
          H.Numerator := num;
          // new "B"
          H.Denominator := denom;
        END;
      //writeln('  End of loop iteration for i = ',i);
    END;
  //return Fraction(intPart, 1) + high;
  //writeln('MF_FloatToFraction: assigning Result');
  //writeln('H = ',H.ToString);
  //writeln('IntPart := ',IntPart);
  //Avoid call to TFraction.Normalize in Result := H + IntPart
  Result := H;
  Result.Numerator := Result.Numerator+ (Result.Denominator * IntPart);
  IF IsNeg THEN
    Result.Numerator := -Result.Numerator;
  //writeln('MF_FloatToFraction End.');
END;


FUNCTION TryFloatToFraction(Value, Precision: Double; out F: TFraction;
                            AcceptPrecisionError: Boolean): Boolean;
BEGIN
  Result := False;
  IF NOT InRange64(Value) THEN Exit;
  AdjustPrecision(Precision, Value);
  TRY
    F := FloatToFraction(Value, Precision);
    Result := AcceptPrecisionError OR (Abs(Value - F.ToFloat) <= Precision)
              EXCEPT
              Result := False;
END
END;

FUNCTION FloatToFractionDef(Value, Precision: Double; Def: TFraction;
                            AcceptPrecisionError: Boolean): TFraction;
BEGIN
  IF NOT TryFloatToFraction(Value, Precision, Result, AcceptPrecisionError) THEN
    Result := Def;
END;


FUNCTION StrToFraction(CONST S: STRING): TFraction;
BEGIN
  IF NOT TryStrToFraction(S, Result) THEN raise EConvertError.CreateFmt(SInvalidFraction, [S]);
END;

{
  S is either:
  A single fraction e.g. 2/3
  A single integer e.g. 10
  An integer + a fraction, separated by a single space (#32) e.g. 2 1/2 (two and a half)
}
FUNCTION TryStrToFraction(CONST S: STRING; out F: TFraction): Boolean;

TYPE 
  TFracType = (ftComplex, ftSingleFraction, ftInteger);

VAR 
  FracType: TFracType;
  SInt, SNum, SDen: String;
  IntPart, NumPart, DenPart: Integer;
  PSpace, PFrac: SizeInt;
BEGIN
  Result := False;
  SInt := '';
  SNum := '';
  SDen := '';
  IntPart := 0;
  NumPart := 0;
  DenPart := 0;
  PSpace := Pos(#32, S);
  PFrac := Pos(FracSymbol,S);
  IF (PSpace > 0) AND (PFrac > 0) THEN FracType := ftComplex
  ELSE IF (PSpace = 0) AND (PFrac > 0) THEN FracType := ftSingleFraction
  ELSE FracType := ftInteger;
  CASE FracType OF 
    ftComplex:
               BEGIN
                 //writeln('ftComplex');
                 SInt := Copy(S, 1, PSpace - 1);
                 SNum := Copy(S, PSpace + 1, PFrac - PSpace - 1);
                 SDen := Copy(S, PFrac + Length(FracSymbol), MaxInt);
                 //no spaces allowed in SNum or SDen
                 //writeln('SInt = "',sint,'" SNum = "',snum,'" SDen = "',sden,'"');
                 IF (Pos(#32, SNum) > 0) OR (Pos(#32, SDen) > 0) THEN Exit;
                 IF NOT (TryStrToInt(SInt, IntPart) AND TryStrToInt(SNum, NumPart) AND TryStrToInt(SDen, DenPart))
                    OR (DenPart = 0) OR (NumPart < 0) THEN
                   Exit;
                 IF (DenPart < 0) THEN
                   BEGIN
                     DenPart := -DenPart;
                     NumPart := -NumPart;
                   END;
                 IF (IntPart = 0) THEN
                   BEGIN
                     F.Numerator := NumPart;
                     F.Denominator := DenPart;
                   END
                 ELSE
                   BEGIN
                     IF (NumPart < 0) THEN
                       BEGIN
                         NumPart := -NumPart;
                         IntPart := -IntPart;
                       END;
                     IF (IntPart > 0) THEN
                       F.Numerator := NumPart + Int64(DenPart) * IntPart
                     ELSE
                       F.Numerator := -(NumPart - Int64(DenPart) * IntPart);
                     F.Denominator := DenPart;
                   END;
               END;
    ftSingleFraction:
                      BEGIN
                        //writeln('ftSingleFraction');
                        SNum := Copy(S, 1, PFrac - 1);
                        SDen := Copy(S, PFrac + Length(FracSymbol), MaxInt);
                        //writeln('SNum = "',snum,'" SDen = "',sden,'"');
                        //no spaces allowed in SNum or SDen
                        IF (Pos(#32, SNum) > 0) OR (Pos(#32, SDen) > 0) THEN Exit;
                        IF NOT (TryStrToInt(SNum, NumPart) AND TryStrToInt(SDen, DenPart))
                           OR (DenPart = 0) THEN
                          Exit;
                        F.Numerator := NumPart;
                        F.Denominator := DenPart;
                      END;
    ftInteger:
               BEGIN
                 //writeln('ftInteger');
                 //no spaces allowed in SInt
                 IF (Pos(#32, SInt) > 0) THEN Exit;
                 IF NOT TryStrToInt(S, IntPart) THEN Exit;
                 F.Numerator := IntPart;
                 F.Denominator := 1;
               END;
  END;
  //case
  Result := True;
END;

FUNCTION StrToFractionDef(CONST S: STRING; Def: TFraction): TFraction;
BEGIN
  IF NOT TryStrToFraction(S, Result) THEN Result := Def;
END;


FUNCTION Min(a, b: TFraction): TFraction;
BEGIN
  IF (a < b) THEN
    Result := b
  ELSE
    Result := b;
END;

FUNCTION Max(a, b: TFraction): TFraction;
BEGIN
  IF (a > b) THEN
    Result := a
  ELSE
    Result := b;
END;

FUNCTION InRange(CONST AValue, AMin, AMax: TFraction): Boolean;
BEGIN
  Result := (AValue >= AMin) AND (AValue <= AMax);
END;

FUNCTION EnsureRange(CONST AValue, AMin, AMax: TFraction): TFraction;
BEGIN
  IF (AValue < AMin) THEN
    Result := AMin
  ELSE IF (AValue > AMax) THEN
         Result := AMax
  ELSE
    Result := AValue;
END;

FUNCTION Sign(CONST AValue: TFraction): TValueSign;
BEGIN
  IF (AValue.Denominator < 0) THEN
    BEGIN
      AValue.Numerator := -AValue.Numerator;
      AValue.Denominator := -AValue.Denominator;
    END;
  IF (AValue.Numerator < 0) THEN
    Result := NegativeValue
  ELSE IF (AValue.Numerator > 0) THEN
         Result := PositiveValue
  ELSE
    Result := ZeroValue;
END;

FUNCTION IsZero(CONST AValue: TFraction): Boolean;
BEGIN
  Result := (AValue.Numerator = 0);
END;

FUNCTION Abs(CONST AValue: TFraction): TFraction;
BEGIN
  Result := AValue;
  Result.Normalize;
  IF (Result.Numerator < 0) THEN Result.Numerator := -Result.Numerator;
END;


FUNCTION Fraction(ANumerator, ADenominator: Int64): TFraction;
BEGIN
  Result.Numerator := ANumerator;
  //will raise exception if ADenominator = 0
  Result.Denominator := ADenominator;
END;

FUNCTION Fraction(AIntPart, ANumerator, ADenominator: Int64): TFraction;

VAR 
  IsNeg: Boolean;
BEGIN
  IF (ANumerator < 0) THEN
    raise EMathError.CreateFmt(SInvalidFraction,[Format('%d %d/%d',[AIntPart,ANumerator,ADenominator])]);
  IsNeg := (AIntPart < 0) xor (ADenominator < 0);
  AIntPart := Abs(AIntPart);
  ADenominator := Abs(ADenominator);
  Result.Numerator := ANumerator + (ADenominator * AIntPart);
  IF IsNeg THEN Result.Numerator := - Result.Numerator;
  //will raise exception if ADenominator = 0
  Result.Denominator := ADenominator;
END;



operator = (F1: TFraction; F2: TFraction) B: Boolean;
BEGIN
  F1.Normalize;
  F2.Normalize;
  B := (F1.Numerator = F2.Numerator) AND (F1.Denominator = F2.Denominator);
END;

operator < (F1: TFraction; F2: TFraction) B: Boolean;
BEGIN
  F1.Normalize;
  F2.Normalize;
  B := (F1.Numerator * F2.Denominator) < (F2.Numerator * F1.Denominator);
END;

operator > (F1: TFraction; F2: TFraction) B: Boolean;
BEGIN
  F1.Normalize;
  F2.Normalize;
  B := (F1.Numerator * F2.Denominator) > (F2.Numerator * F1.Denominator);
END;

operator <= (F1: TFraction; F2: TFraction) B: Boolean;
BEGIN
  B := (F1 < F2) OR (F1 = F2);
END;

operator >= (F1: TFraction; F2: TFraction) B: Boolean;
BEGIN
  B := (F1 > F2) OR (F1 = F2);
END;

operator := (I: Int64) F: TFraction;
BEGIN
  F.Numerator := I;
  F.Denominator := 1;
END;

operator := (S: STRING) F: TFraction;
BEGIN
  IF NOT TryStrToFraction(S, F) THEN
    raise EConvertError.CreateFmt(SInvalidFraction, [S]);
END;


operator + (L: TFraction; R: TFraction) F: TFraction;
BEGIN
  F.Numerator := L.Numerator * R.Denominator + R.Numerator * L.Denominator;
  F.Denominator := L.Denominator * R.Denominator;
  F.Normalize;
END;


operator - (L: TFraction; R: TFraction) F: TFraction;
BEGIN
  R.Numerator := - R.Numerator;
  F := L + R;
END;


operator - (L: TFraction) F: TFraction;
BEGIN
  F.Numerator := - L.Numerator;
  F.Denominator := L.Denominator;
  F.Normalize;
END;

operator * (L: TFraction; R: TFraction) F: TFraction;
BEGIN
  L.Normalize;
  R.Normalize;
  F.Numerator := L.Numerator * R.Numerator;
  F.Denominator := L.Denominator * R.Denominator;
  F.Normalize;
END;

operator * (L: TFraction; R: Int64) F: TFraction;
BEGIN
  F := L;
  F.Normalize;
  F.Numerator := L.Numerator * R;
  F.Normalize;
END;

operator * (L: Int64; R: TFraction) F: TFraction;
BEGIN
  F := R * L;
END;

operator / (L: TFraction; R: TFraction) F: TFraction;

VAR 
  Temp: TFraction;
BEGIN
  Temp.Numerator := R.Denominator;
  //this will raise an exception if R = 0
  Temp.Denominator := R.Numerator;
  F := L * Temp;
END;

operator / (L: TFraction; R: Int64) F: TFraction;
BEGIN
  IF (R = 0) OR (L.Denominator = 0) THEN raise EZeroDivide.Create(SDivisionByZero);
  F := L;
  F.Normalize;
  F.Denominator := F.Denominator * R;
  F.Normalize;
END;

operator ** (L: TFraction; R: Integer) F: TFraction;

VAR 
  i: Integer;
BEGIN
  L.Normalize;
  F := L;
  IF (R = 0) THEN
    BEGIN
      IF (L.Numerator = 0) AND NOT AllowZeroPowerZero THEN raise EMathError.Create(SZeroPowerZero);
      F.Numerator := 1;
      F.Denominator := 1;
    END
  ELSE IF (R > 0) THEN
         BEGIN
           FOR i := 1 TO R-1 DO
             F := F * L
         END
  ELSE
    BEGIN
      F := 1 / F;
      L := F;
      FOR i := 1 TO -(R)-1 DO
        F := F * L ;
    END;
END;


PROCEDURE TFraction.SetDominator(AValue: Int64);
BEGIN
  IF (AValue = 0) THEN raise EZeroDivide.Create(SDivisionByZero);
  IF (FDenominator = AValue) THEN Exit;
  FDenominator := AValue;
END;

PROCEDURE TFraction.Normalize;

VAR 
  GCD: Int64;
BEGIN
  IF (Denominator < 0) THEN
    BEGIN
      Numerator := - Numerator;
      Denominator := - Denominator;
    END;
  GCD := GreatestCommonDivisor(Numerator, Denominator);
  IF (GCD <> 1) THEN
    BEGIN
      Numerator := Numerator DIV GCD;
      Denominator := Denominator DIV GCD
    END;
END;

FUNCTION TFraction.ToString: String;
BEGIN
  IF (Denominator < 0) THEN
    BEGIN
      Numerator := - Numerator;
      Denominator := - Denominator;
    END;
  Result := IntToStr(Numerator) + FracSymbol + IntToStr(Denominator);
END;

FUNCTION TFraction.Resolve: String;

VAR 
  Num, IntPart: Int64;
BEGIN
  Normalize;
  IF (Abs(Numerator) > Abs(Denominator)) THEN
    BEGIN
      IntPart := Numerator DIV Denominator;
      Num := Numerator MOD Denominator;
      IF (IntPart < 0) THEN Num := -Num;
      IF (Num <> 0) THEN
        Result := IntToStr(IntPart) + #32 + IntToStr(Num) +  FracSymbol + IntToStr(Denominator)
      ELSE
        Result := IntToStr(IntPart);
    END
  ELSE
    BEGIN
      Result := ToString;
    END;
END;

FUNCTION TFraction.ToFloat: Double;
BEGIN
  Result := Numerator / Denominator;
END;

END.
