unit uLanguages;

interface

uses
 System.Classes, System.SysUtils, System.JSon;

type
 [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidAndroid)]
 TLanguage = class(TComponent)
  private
   FLang: string;
   FCurrLang: string;
   FIds: TJSONArray;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   //set resource/language
   procedure setSource(const resName: string);
   procedure setLanguage(const langId: string = 'Default');
   //localization function
   function localize(const id: string): string;
 end;

procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('AlbertoComp.', [TLanguage]);
end;

{ TLanguage }

constructor TLanguage.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FLang := ' ';
 FCurrLang := 'Default';
end;

destructor TLanguage.Destroy;
begin
 FIds.Free;
 inherited;
end;

function TLanguage.localize(const id: string): string;
var
 FValue, FValueInner, FValueData: TJSONValue;
begin

 Result := '';

 try

  for FValue in FIds do
   begin

    if (FValue is TJSONObject) then
     begin

      FValueInner := TJSONObject(FValue).Values[FCurrLang];
      if (FValueInner is TJSONArray) then
       begin

        for FValueData in TJSONArray(FValueInner) do
         begin
          Result := FvalueData.GetValue<string>(id);
         end;

       end;

     end;

   end;

 except
  Result := '-';
 end;

end;

procedure TLanguage.setLanguage(const langId: string);
begin
  FCurrLang := langId;
end;

procedure TLanguage.setSource(const resName: string);
var
 rs: TResourceStream;
 sl: TStringList;
begin

 FLang := resName;

 //in System.Types > const RT_RCDATA = PChar(10);
 rs := TResourceStream.Create(HInstance, FLang, PChar(10));
 try

  sl := TStringList.Create;
  try

    sl.LoadFromStream(rs, TEncoding.UTF8);
    FIds := TJSONObject.ParseJSONValue(sl.Text) as TJSONArray;

  finally
   sl.Free;
  end;

 finally
  rs.Free;
 end;

end;

end.
