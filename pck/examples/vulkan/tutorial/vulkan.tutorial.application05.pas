unit vulkan.tutorial.application05;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  vulkan.tutorial.application04,
  pax.glfw, pax.vulkan,
  pax.vulkan.helpers;

type
  { TVulkanApplicationTutorial05 }

  TVulkanApplicationTutorial05 = class(TVulkanApplicationTutorial04)
  protected
    FSurface: VkSurfaceKHR;
    FPresentQueue: VkQueue;
    FSurfaceExtension: IVulkanKHRSurface;
  protected
    procedure InitVulkan; override;
    procedure Cleanup; override;
    procedure CreateSurface;
  public
    constructor Create; override;
  end;

implementation

{ TVulkanApplicationTutorial05 }

constructor TVulkanApplicationTutorial05.Create;
begin
  getLogger.Enter('TVulkanApplicationTutorial05', 'Create');
  inherited Create;
  FSurface := VK_NULL_HANDLE;
  FPresentQueue := VK_NULL_HANDLE;
  Caption := 'Vulkan Tutorial 05 - Window Surface';
  getLogger.Leave('TVulkanApplicationTutorial05', 'Create');
end;

procedure TVulkanApplicationTutorial05.CreateSurface;
begin
  getLogger.Enter('TVulkanApplicationTutorial05', 'CreateSurface');
  with getGLFW do
  begin
    if glfwCreateWindowSurface(FInstance, FWindow, nil, FSurface) <> VK_SUCCESS then
      raise Exception.Create('Impossibile creare superficie finestra');
  end;
  FSurfaceExtension := getVulkan.getKHRSurfaceExtension(FInstance);
  getLogger.Leave('TVulkanApplicationTutorial05', 'CreateSurface');
end;

procedure TVulkanApplicationTutorial05.InitVulkan;
begin
  getLogger.Enter('TVulkanApplicationTutorial05', 'InitVulkan');
  inherited InitVulkan;
  CreateSurface;
  getLogger.Leave('TVulkanApplicationTutorial05', 'InitVulkan');
end;

procedure TVulkanApplicationTutorial05.Cleanup;
begin
  getLogger.Enter('TVulkanApplicationTutorial05', 'Cleanup');
  if FSurface <> VK_NULL_HANDLE then
    FSurfaceExtension.vkDestroySurfaceKHR(FSurface, nil);
  inherited Cleanup;
  getLogger.Leave('TVulkanApplicationTutorial05', 'Cleanup');
end;

end.
