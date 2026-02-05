unit vulkan.tutorial.application12;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pax.glfw,
  pax.vulkan,
  pax.vulkan.helpers,
  vulkan.tutorial,
  vulkan.tutorial.application11;

type
  { TVulkanApplicationTutorial12 }

  TVulkanApplicationTutorial12 = class(TVulkanApplicationTutorial11, IVulkanTutorial)
  protected
    FFramebufferResized: boolean;
  protected
    procedure InitWindow; override;
    procedure InitVulkan; override;
    procedure MainLoop; override;
    procedure Cleanup; override;
    procedure CleanupSwapChain;
    procedure RecreateSwapChain;
  public
    constructor Create; override;
  end;

procedure FramebufferResizeCallback(window: PGLFWwindow; Width, Height: integer); cdecl;

implementation

procedure FramebufferResizeCallback(window: PGLFWwindow; Width, Height: integer); cdecl;
var
  app: TVulkanApplicationTutorial12;
begin
  app := TVulkanApplicationTutorial12(getGLFW.glfwGetWindowUserPointer(window));
  app.FFramebufferResized := True;
end;

{ TVulkanApplicationTutorial12 }

constructor TVulkanApplicationTutorial12.Create;
begin
  inherited Create;
  FFramebufferResized := False;
  Caption := 'Vulkan Tutorial 12 - Swap Chain Recreation';
end;

procedure TVulkanApplicationTutorial12.InitWindow;
begin
  inherited InitWindow;
  getGLFW.glfwSetWindowUserPointer(FWindow, Pointer(Self));
  getGLFW.glfwSetFramebufferSizeCallback(FWindow, @FramebufferResizeCallback);
  getGLFW.glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE);
end;

procedure TVulkanApplicationTutorial12.CleanupSwapChain;
var
  i: integer;
begin
  with getVulkan do
  begin
    for i := 0 to High(FFramebuffers) do
      vkDestroyFramebuffer(FDevice, FFramebuffers[i], nil);
    for i := 0 to High(FSwapChainImageViews) do
      vkDestroyImageView(FDevice, FSwapChainImageViews[i], nil);
    FSwapChainExtension.vkDestroySwapchainKHR(FSwapChainHandle, nil);
  end;
end;

procedure TVulkanApplicationTutorial12.RecreateSwapChain;
begin
  with getVulkan do vkDeviceWaitIdle(FDevice);
  CleanupSwapChain;
  CreateSwapChain;
  CreateImageViews;
  CreateFramebuffers;
end;

procedure TVulkanApplicationTutorial12.InitVulkan;
begin
  inherited InitVulkan;
end;

procedure TVulkanApplicationTutorial12.MainLoop;
begin
  with getGLFW do
    while not glfwWindowShouldClose(FWindow) do
    begin
      glfwPollEvents;
      if FFramebufferResized then
      begin
        FFramebufferResized := False;
        RecreateSwapChain;
      end;
      DrawFrame;
    end;
end;

procedure TVulkanApplicationTutorial12.Cleanup;
begin
  CleanupSwapChain;
  inherited Cleanup;
end;

end.
