unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.MultiView, FMX.Edit,
  FMX.Grid.Style, FMX.ScrollBox, FMX.Grid, System.IOUtils, System.Rtti,
  FMX.TabControl, System.JSON, System.JSON.Readers, System.JSON.Types,
  FMX.ComboEdit, System.JSON.Writers, System.ImageList, FMX.ImgList;

type TStringGridAccess = class(TCustomGrid)
end;

type
  TFormMain = class(TForm)
    TopBar: TRectangle;
    Label1: TLabel;
    Layout1: TLayout;
    Browse: TButton;
    EditPath: TEdit;
    OpenDialog: TOpenDialog;
    Ok: TButton;
    StringGrid: TStringGrid;
    SaveEdits: TButton;
    Layout2: TLayout;
    GridPanelLayout1: TGridPanelLayout;
    Layout3: TLayout;
    AddLanguage: TButton;
    NewLangName: TEdit;
    Layout4: TLayout;
    DeleteButton: TButton;
    Defaults: TComboEdit;
    Layout5: TLayout;
    GridPanelLayout2: TGridPanelLayout;
    Layout6: TLayout;
    AddNewWord: TButton;
    NewWord: TEdit;
    Layout7: TLayout;
    DeleteWord: TButton;
    Words: TComboEdit;
    StringColumn1: TStringColumn;
    ImageList: TImageList;
    Layout8: TLayout;
    Rectangle1: TRectangle;
    Layout9: TLayout;
    Button1: TButton;
    SaveDialog: TSaveDialog;
    procedure BrowseClick(Sender: TObject);
    procedure OkClick(Sender: TObject);
    procedure AddLanguageClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure AddNewWordClick(Sender: TObject);
    procedure DeleteWordClick(Sender: TObject);
    procedure SaveEditsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure parseJSONItem(const text: string; col: TStringColumn);
    procedure LoadFile;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.AddLanguageClick(Sender: TObject);
var a: TColumn;
begin

  if NewLangName.Text.Length > 0 then
   begin
     a := TStringColumn.Create(StringGrid);
     a.Header := NewLangName.Text;
     StringGrid.AddObject(a);

     Defaults.Items.Append(NewLangName.Text);
     NewLangName.Text := '';
   end
  else
   begin
     ShowMessage('Invalid input. You have to enter at least 1 character!');
   end;

end;

procedure TFormMain.BrowseClick(Sender: TObject);
begin

 if OpenDialog.Execute then
  EditPath.Text := OpenDialog.FileName;

end;

procedure TFormMain.Button1Click(Sender: TObject);
begin

 if SaveDialog.Execute then
  begin
    EditPath.Text := SaveDialog.FileName;
    LoadFile;
  end;

end;

procedure TFormMain.AddNewWordClick(Sender: TObject);
begin

  if NewWord.Text.Length > 0 then
   begin
    StringGrid.RowCount := StringGrid.RowCount + 1;
    StringGrid.Cells[0, StringGrid.RowCount-1] := NewWord.Text;
    Words.Items.Append(NewWord.Text);
    NewWord.Text := '';
   end
  else
   begin
    ShowMessage('Invalid input. You have to enter at least 1 character!');
   end;

end;

procedure TFormMain.DeleteWordClick(Sender: TObject);
var i,j: integer;
begin

 if Words.Items.Count > 1 then
  begin

   StringGrid.BeginUpdate;
   try

    for i := (Words.ItemIndex-1) to (StringGrid.RowCount - 2) do
     for j := 0 to StringGrid.ColumnCount-1 do
      StringGrid.Cells[j, i] := StringGrid.Cells[j, i+1];

     StringGrid.RowCount := StringGrid.RowCount - 1;

   finally
    StringGrid.EndUpdate;
   end;

   Words.Items.Delete(Words.ItemIndex);
   Words.ItemIndex := 0;

  end;

end;

procedure TFormMain.LoadFile;
var
 Fjson: TStringList;
 Farr: TJSONArray;
 FValue: TJSONValue;
 aCol: TStringColumn;
 k: integer;
begin

 if TFile.Exists(EditPath.Text) then
  begin

   //clear the string grid
   for k := 1 to StringGrid.ColumnCount-1 do
    begin
     StringGrid.Columns[k].DisposeOf;
    end;

   k := 0;
   StringGrid.BeginUpdate;

   //start the json parsing
   FJson := TStringList.Create;
   try

    FJson.LoadFromFile(EditPath.Text, TEncoding.UTF8);

    if (FJson.Count > 0) then
     begin

      Farr := TJSONObject.ParseJSONValue(FJson.Text) as TJSONArray;

      for FValue in FArr do
       begin

        if (FValue is TJSONObject) then
         begin

          if k > 0 then
           begin
            aCol := TStringColumn.Create(StringGrid);
            StringGrid.AddObject(aCol);
           end
          else
           begin
            aCol := (StringGrid.Columns[0] as TStringColumn);
           end;

          parseJSONItem(TJSONObject(FValue).ToString, aCol);

         end;

         Inc(k);

       end;

     end;

   finally
    FJson.Free;
   end;

   //default text in the combo boxes
   if (Defaults.Items.Count >= 0) then
    Defaults.ItemIndex := 0
   else
    Defaults.ItemIndex := -1;

   if (Words.Items.Count >= 0) then
    Words.ItemIndex := 0
   else
    Defaults.ItemIndex := -1;

   StringGrid.EndUpdate;

  end
 else
  begin
   TFile.WriteAllText(EditPath.Text, '');
  end;

 AddLanguage.Enabled := true;
 AddNewWord.Enabled := true;
 DeleteButton.Enabled := true;
 DeleteWord.Enabled := true;
 SaveEdits.Enabled := true;

end;

procedure TFormMain.DeleteButtonClick(Sender: TObject);
begin

 if (Defaults.Items.Count > 1) then
  begin
   StringGrid.Columns[Defaults.ItemIndex].DisposeOf;
   Defaults.Items.Delete(Defaults.ItemIndex);
   Defaults.ItemIndex := 0;
  end;

end;

procedure TFormMain.OkClick(Sender: TObject);
begin
  LoadFile;
end;

procedure TFormMain.parseJSONItem(const text: string; col: TStringColumn);
var
  LStringReader: TStringReader;
  LJSONTextReader: TJsonTextReader;
  index: integer;
  tmp: string;
begin

 LStringReader := TStringReader.Create(text);
  try

   LJsonTextReader := TJsonTextReader.Create(LStringReader);
   try

    index := 0;

    while LJsonTextReader.Read do
     begin

      if col.Header.IsEmpty then
       begin
        tmp := text;
        Delete(tmp, 1, 2);
        Delete(tmp, Pos('"', tmp), tmp.Length-1);
        col.Header := tmp;
        Defaults.Items.Append(' ' + tmp);
       end;

      case LJsonTextReader.TokenType of

       TJsonToken.PropertyName:
        begin

         if StringGrid.Cells[0,index].IsEmpty then
          begin
           StringGrid.RowCount := StringGrid.RowCount + 1;
           StringGrid.Cells[0,index] := LJsonTextReader.Value.AsString;
           Words.Items.Append(LJsonTextReader.Value.AsString);
          end;

        end;

       TJsonToken.String:
        begin
         StringGrid.Cells[StringGrid.ColumnCount-1,index] := LJsonTextReader.Value.AsString;
         Inc(index);
        end;

      end;

     end;

   finally
    LJsonTextReader.Free;
   end;

  finally
   LStringReader.Free;
  end;

end;

procedure TFormMain.SaveEditsClick(Sender: TObject);
var
 TextWriter: TStringWriter;
 Writer: TJsonTextWriter;
 i, j: integer;
begin

 TextWriter := TStringWriter.Create;
 try

   Writer := TJsonTextWriter.Create(TextWriter);
   try

    Writer.Formatting := TJsonFormatting.None;
    Writer.WriteStartArray;

    for i := 0 to StringGrid.ColumnCount-1 do
     begin

      Writer.WriteStartObject;
      Writer.WritePropertyName(StringGrid.Columns[i].Header);
      Writer.WriteStartArray;
      Writer.WriteStartObject;

      for j := 0 to StringGrid.RowCount-1 do
       begin
        Writer.WritePropertyName(StringGrid.Cells[0,j]);
        Writer.WriteValue(StringGrid.Cells[i,j]);
       end;

      Writer.WriteEndObject;
      Writer.WriteEndArray;
      Writer.WriteEndObject;

     end;

    Writer.WriteEndArray;

    TFile.WriteAllText(EditPath.Text, UTF8Encode(TextWriter.ToString));

   finally
    Writer.Free;
   end;

 finally
  TextWriter.Free;
 end;

end;

end.
