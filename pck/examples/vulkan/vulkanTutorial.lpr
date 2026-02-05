program vulkanTutorial;

{$mode objfpc}{$H+}
{$packenum 4}
{$packrecords C}
{$define VK_VERSION_1_0}
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  pax.glfw,
  pax.vulkan,
  vulkan.tutorial,
  vulkan.tutorial.application00,
  vulkan.tutorial.application01,
  vulkan.tutorial.application02,
  vulkan.tutorial.application03,
  vulkan.tutorial.application04,
  vulkan.tutorial.application05,
  vulkan.tutorial.application06,
  vulkan.tutorial.application07,
  vulkan.tutorial.application08,
  vulkan.tutorial.application09,
  vulkan.tutorial.application10,
  vulkan.tutorial.application11,
  vulkan.tutorial.application12,
  vulkan.tutorial.application13,
  vulkan.tutorial.application14,
  vulkan.tutorial.application15,
  vulkan.tutorial.application16,
  vulkan.tutorial.application17,
  vulkan.tutorial.application18,
  vulkan.tutorial.application19,
  logger,
  pax.vulkan.helpers;

var
  application: IVulkanTutorial = nil;
  idx: integer = 1;
  EnableValidationLayers: boolean = False;
  param: string;
begin
  while idx <= ParamCount do
  begin
    param := LowerCase(ParamStr(idx));
    if param = '-d' then
      EnableValidationLayers := True;
    if param = '-t' then
    begin
      case ParamStr(idx + 1) of
        '1': begin
          application := TVulkanApplicationTutorial01.Create;
          Inc(idx);
        end;
        '2': begin
          application := TVulkanApplicationTutorial02.Create;
          Inc(idx);
        end;
        '3': begin
          application := TVulkanApplicationTutorial03.Create;
          Inc(idx);
        end;
        '4': begin
          application := TVulkanApplicationTutorial04.Create;
          Inc(idx);
        end;
        '5': begin
          application := TVulkanApplicationTutorial05.Create;
          Inc(idx);
        end;
        '6': begin
          application := TVulkanApplicationTutorial06.Create;
          Inc(idx);
        end;
        '7': begin
          application := TVulkanApplicationTutorial07.Create;
          Inc(idx);
        end;
        '8': begin
          application := TVulkanApplicationTutorial08.Create;
          Inc(idx);
        end;
        '9': begin
          application := TVulkanApplicationTutorial09.Create;
          Inc(idx);
        end;
        '10': begin
          application := TVulkanApplicationTutorial10.Create;
          Inc(idx);
        end;
        '11': begin
          application := TVulkanApplicationTutorial11.Create;
          Inc(idx);
        end;
        '12': begin
          application := TVulkanApplicationTutorial12.Create;
          Inc(idx);
        end;
        '13': begin
          application := TVulkanApplicationTutorial13.Create;
          Inc(idx);
        end;
        '14': begin
          application := TVulkanApplicationTutorial14.Create;
          Inc(idx);
        end;
        '15': begin
          application := TVulkanApplicationTutorial15.Create;
          Inc(idx);
        end;
        '16': begin
          application := TVulkanApplicationTutorial16.Create;
          Inc(idx);
        end;
        '17': begin
          application := TVulkanApplicationTutorial17.Create;
          Inc(idx);
        end;
        '18': begin
          application := TVulkanApplicationTutorial18.Create;
          Inc(idx);
        end;
        '19': begin
          application := TVulkanApplicationTutorial19.Create;
          Inc(idx);
        end

        else
          application := nil;
      end;
    end;
    Inc(idx);
  end;
  try
    if application = nil then
      application := TVulkanApplicationTutorial00.Create;

    application.Logger.info('Avviato tutorial: %s', [application.Caption]);
    application.EnableValidationLayers := EnableValidationLayers;
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
