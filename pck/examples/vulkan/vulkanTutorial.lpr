program vulkanTutorial;

{$mode objfpc}{$H+}
{$packenum 4}
{$packrecords C}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  pax.glfw,
  pax.vulkan,
  vulkan.tutorial.application;

const
  Width: uint32 = 800;
  Height: uint32 = 600;

var
  application: TVulkanTutorialApplication = nil;

begin
  if ParamStr(1) = '-t' then
    case ParamStr(2) of
      '1': application := TVulkanTutorialApplication01.Create;
      '2': application := TVulkanTutorialApplication02.Create;
      '3': application := TVulkanTutorialApplication03.Create;
      '4': application := TVulkanTutorialApplication04.Create;
      else
        application := nil;
    end;
  try
    if application = nil then
      application := TVulkanTutorialApplication00.Create;

    application.Width := Width;
    application.Height := Height;
    application.run();
  except
    on e: Exception do
    begin
      WriteLn(e.Message);
      halt(1);
    end;
  end;

  ExitCode := 0;
end.
