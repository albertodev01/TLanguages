# TLanguages

This is a component written in Delphi that works with VCL and FMX and can be used to localize (translate) your Windows/iOS/Android applications. You'll have to install a component in the IDE in order to load the localized strings stored in a `.json` file; this file will be created using the apposite Editor.

<p align="center">
  <img src="https://github.com/albertodev01/TLanguages/blob/master/editor.png" />
</p>

There is a Default language which contains the IDs of the strings. You can add using the apposite buttons new words or language and once you're done store your work in a json file. In the sections below I'll show you how to install everything properly and then run an example. 

# Install

To install the component just follow these steps:

1. Download this repo and open ProjectLanguages.dproj in the Component folder
2. In the Project Manager (on the top right) double click `ProjectLanguages.bpl` to select it
3. Now `ProjectLanguages.bpl` should be selected. Right click and do `Compile`, then `Build` and finally `Install`

Do not forget to add the source files to the library path of the IDE. To do this go on Tools > Options > Delphi Options > Library and for each platform add the path to the folder containing the sources of the component. 

Now you just need to open the Editor folder, open `ProjectLanguages.dproj` and then give a Compile > Build to create the executable of the Editor for your machine. The editor has been written with FMX so it can be compiled on Win32, Win64 and OS X!

# Usage

1. Open the editor and create a New Language File using the apposite button above (or use Browse to open a file if you had already created one). Now you can use the buttons to add words/languages and the table to edit the values that you want to assign to the localized strings. I have created this as example:

<p align="center"><img src="https://github.com/albertodev01/TLanguages/blob/master/editor_test.png" /></p>

2. Once you have finished, click Save Edits and open Delphi. Create a new project (VCL or FMX), drop the `Language1` component in the form. Just as test, add a TLabel component in the form.

3. Now go on Project > Resources and Images > Add... > select the `.json` file generated with the Editor > give it an Identifier > select RCDATA as type > click Ok.

Now you have finished the setup and you can start using the component. This code shows how to initialize the component in the `FormCreate` and then how to localize strings.

``` pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  Language1.setSource('jsonResourceId');
  Language1.setLanguage('Default');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin

  Language1.setLanguage('it');
  //The caption is now 'casa'
  Label1.Caption := Language1.localize('home');

  Language1.setLanguage('fr');
  //The caption is now 'maison'
  Label1.Caption := Language1.localize('home');

end;
```
