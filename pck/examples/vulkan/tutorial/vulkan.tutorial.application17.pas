unit vulkan.tutorial.application17;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pax.vulkan,
  pax.vulkan.helpers,
  vulkan.tutorial,
  vulkan.tutorial.application16;

const
  MAX_FRAMES_IN_FLIGHT = 2;

type
  { TVulkanApplicationTutorial17 }

  TVulkanApplicationTutorial17 = class(TVulkanApplicationTutorial16, IVulkanTutorial)
  protected
    FUniformBuffers: array[0..MAX_FRAMES_IN_FLIGHT - 1] of VkBuffer;
    FUniformBuffersMemory: array[0..MAX_FRAMES_IN_FLIGHT - 1] of VkDeviceMemory;
    FDescriptorSetLayout: VkDescriptorSetLayout;
  protected
    procedure InitVulkan; override;
    procedure Cleanup; override;
    procedure CreateDescriptorSetLayout;
    procedure CreateUniformBuffers;
    procedure UpdateUniformBuffer(currentImage: uint32);
    procedure DrawFrame;
  public
    constructor Create; override;
  end;

implementation

uses
  pax.glfw;

  { TVulkanApplicationTutorial17 }

constructor TVulkanApplicationTutorial17.Create;
begin
  inherited Create;
  Caption := 'Vulkan Tutorial 17 - Descriptor Layout and Buffer';
end;

procedure TVulkanApplicationTutorial17.CreateDescriptorSetLayout;
var
  uboLayoutBinding: VkDescriptorSetLayoutBinding;
  layoutInfo: VkDescriptorSetLayoutCreateInfo;
begin
  uboLayoutBinding.zero;
  uboLayoutBinding.binding := 0;
  uboLayoutBinding.descriptorType := VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
  uboLayoutBinding.descriptorCount := 1;
  uboLayoutBinding.stageFlags := VK_SHADER_STAGE_VERTEX_BIT;
  uboLayoutBinding.pImmutableSamplers := nil;

  layoutInfo.zero;
  layoutInfo.sType := VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
  layoutInfo.bindingCount := 1;
  layoutInfo.pBindings := @uboLayoutBinding;

  with getVulkan do
    if vkCreateDescriptorSetLayout(FDevice, @layoutInfo, nil, @FDescriptorSetLayout) <> VK_SUCCESS then
      raise Exception.Create('Impossibile creare descriptor set layout');
end;

procedure TVulkanApplicationTutorial17.CreateUniformBuffers;
var
  bufferSize: VkDeviceSize;
  i: integer;
begin
  bufferSize := SizeOf(TUniformBufferObject);

  for i := 0 to MAX_FRAMES_IN_FLIGHT - 1 do
    CreateBuffer(bufferSize, VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT or VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, FUniformBuffers[i], FUniformBuffersMemory[i]);
end;

procedure TVulkanApplicationTutorial17.UpdateUniformBuffer(currentImage: uint32);
var
  Data: Pointer;
  ubo: TUniformBufferObject;
  time: single;
begin
  time := getGLFW.glfwGetTime;
  ubo.model := Mat4Rotate(Mat4Identity, time * DegToRad * 90, TVec3.Create(0, 0, 1));
  ubo.view := Mat4LookAt(TVec3.Create(2, 2, 2), TVec3.Create(0, 0, 0), TVec3.Create(0, 0, 1));
  ubo.proj := Mat4Perspective(45, FSwapChainExtent.Width / FSwapChainExtent.Height, 0.1, 10);
  ubo.proj.m[1, 1] := ubo.proj.m[1, 1] * -1;  // Flip Y per Vulkan

  with getVulkan do
  begin
    vkMapMemory(FDevice, FUniformBuffersMemory[currentImage], 0, SizeOf(ubo), 0, @Data);
    Move(ubo, Data^, SizeOf(ubo));
    vkUnmapMemory(FDevice, FUniformBuffersMemory[currentImage]);
  end;
end;

procedure TVulkanApplicationTutorial17.DrawFrame;
begin
  UpdateUniformBuffer(FCurrentFrame);
  inherited DrawFrame;
end;

procedure TVulkanApplicationTutorial17.InitVulkan;
begin
  inherited InitVulkan;
  CreateDescriptorSetLayout;
  CreateUniformBuffers;
end;

procedure TVulkanApplicationTutorial17.Cleanup;
var
  i: integer;
begin
  with getVulkan do
  begin
    vkDestroyDescriptorSetLayout(FDevice, FDescriptorSetLayout, nil);
    for i := 0 to MAX_FRAMES_IN_FLIGHT - 1 do
    begin
      vkDestroyBuffer(FDevice, FUniformBuffers[i], nil);
      vkFreeMemory(FDevice, FUniformBuffersMemory[i], nil);
    end;
  end;
  inherited Cleanup;
end;

end.
