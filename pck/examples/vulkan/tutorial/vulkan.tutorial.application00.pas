unit vulkan.tutorial.application00;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils,
  pax.vulkan,
  vulkan.tutorial,
  pax.glfw;

const
  Width = 800;
  Height = 600;

type
  { TVulkanApplicationTutorial00 }

  TVulkanApplicationTutorial00 = class(TVulkanTutorial, IVulkanTutorial)
  protected
    FWindow: PGLFWwindow;
  protected
    procedure InitWindow; override;
    procedure InitVulkan; override;
    procedure MainLoop; override;
    procedure Cleanup; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TVulkanApplicationTutorial00 }

constructor TVulkanApplicationTutorial00.Create;
begin
  getLogger.Enter(ClassName, 'Create');
  inherited Create;
  FWindow := nil;
  Caption := 'Vulkan Tutorial 00 - Base Code';
  getLogger.Leave(ClassName, 'Create');
end;

destructor TVulkanApplicationTutorial00.Destroy;
begin
  getLogger.Enter('TVulkanApplicationTutorial00', 'Destroy');
  inherited Destroy;
  getLogger.Leave('TVulkanApplicationTutorial00', 'Destroy');
end;

procedure TVulkanApplicationTutorial00.InitWindow;
begin
  getLogger.Enter('TVulkanApplicationTutorial00', 'InitWindow');
  with getGLFW do
  begin
    if not glfwInit then
      raise Exception.Create('Impossibile inizializzare GLFW!');

    glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);

    FWindow := glfwCreateWindow(Width, Height, PChar(Caption), nil, nil);
    if FWindow = nil then
    begin
      glfwTerminate;
      raise Exception.Create('Impossibile creare la finestra GLFW!');
    end;
  end;
  getLogger.Leave('TVulkanApplicationTutorial00', 'InitWindow');
end;

procedure TVulkanApplicationTutorial00.InitVulkan;
begin
end;

procedure TVulkanApplicationTutorial00.MainLoop;
begin
  getLogger.Enter('TVulkanApplicationTutorial00', 'MainLoop');
  with getGLFW do
  begin
    while not glfwWindowShouldClose(FWindow) do
      glfwPollEvents;
  end;
  getLogger.Leave('TVulkanApplicationTutorial00', 'MainLoop');
end;

procedure TVulkanApplicationTutorial00.Cleanup;
begin
  getLogger.Enter('TVulkanApplicationTutorial00', 'Cleanup');
  with getGLFW do
  begin
    if FWindow <> nil then
    begin
      getLogger.Debug('Destroing Window');
      glfwDestroyWindow(FWindow);
      FWindow := nil;
    end;

    glfwTerminate;
  end;
  getLogger.Leave('TVulkanApplicationTutorial00', 'Cleanup');
end;

end.
