unit vulkan.tutorial.application18;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pax.vulkan,
  pax.vulkan.helpers,
  vulkan.tutorial,
  vulkan.tutorial.application17;

type
  { TVulkanApplicationTutorial18 }

  TVulkanApplicationTutorial18 = class(TVulkanApplicationTutorial17, IVulkanTutorial)
  protected
    FDescriptorPool: VkDescriptorPool;
    FDescriptorSets: array of VkDescriptorSet;
  protected
    procedure InitVulkan; override;
    procedure Cleanup; override;
    procedure CreateDescriptorPool;
    procedure CreateDescriptorSets;
  public
    constructor Create; override;
  end;

implementation

{ TVulkanApplicationTutorial18 }

constructor TVulkanApplicationTutorial18.Create;
begin
  inherited Create;
  Caption := 'Vulkan Tutorial 18 - Descriptor Pool and Sets';
end;

procedure TVulkanApplicationTutorial18.CreateDescriptorPool;
var
  poolSize: VkDescriptorPoolSize;
  poolInfo: VkDescriptorPoolCreateInfo;
begin
  poolSize.zero;
  poolSize.descriptorType := VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
  poolSize.descriptorCount := MAX_FRAMES_IN_FLIGHT;

  poolInfo.zero;
  poolInfo.sType := VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
  poolInfo.poolSizeCount := 1;
  poolInfo.pPoolSizes := @poolSize;
  poolInfo.maxSets := MAX_FRAMES_IN_FLIGHT;

  with getVulkan do
    if vkCreateDescriptorPool(FDevice, @poolInfo, nil, @FDescriptorPool) <> VK_SUCCESS then
      raise Exception.Create('Impossibile creare descriptor pool');
end;

procedure TVulkanApplicationTutorial18.CreateDescriptorSets;
var
  layouts: array of VkDescriptorSetLayout;
  allocInfo: VkDescriptorSetAllocateInfo;
  i: integer;
  descriptorWrite: VkWriteDescriptorSet;
  bufferInfo: VkDescriptorBufferInfo;
begin
  SetLength(layouts, MAX_FRAMES_IN_FLIGHT);
  for i := 0 to High(layouts) do
    layouts[i] := FDescriptorSetLayout;

  allocInfo.zero;
  allocInfo.sType := VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
  allocInfo.descriptorPool := FDescriptorPool;
  allocInfo.descriptorSetCount := MAX_FRAMES_IN_FLIGHT;
  allocInfo.pSetLayouts := @layouts[0];

  SetLength(FDescriptorSets, MAX_FRAMES_IN_FLIGHT);

  with getVulkan do
    if vkAllocateDescriptorSets(FDevice, @allocInfo, @FDescriptorSets[0]) <> VK_SUCCESS then
      raise Exception.Create('Impossibile allocare descriptor sets');

  for i := 0 to MAX_FRAMES_IN_FLIGHT - 1 do
  begin
    bufferInfo.zero;
    bufferInfo.buffer := FUniformBuffers[i];
    bufferInfo.offset := 0;
    bufferInfo.range := SizeOf(TUniformBufferObject);

    descriptorWrite.zero;
    descriptorWrite.sType := VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
    descriptorWrite.dstSet := FDescriptorSets[i];
    descriptorWrite.dstBinding := 0;
    descriptorWrite.dstArrayElement := 0;
    descriptorWrite.descriptorType := VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
    descriptorWrite.descriptorCount := 1;
    descriptorWrite.pBufferInfo := @bufferInfo;

    getVulkan.vkUpdateDescriptorSets(FDevice, 1, @descriptorWrite, 0, nil);
  end;
end;

procedure TVulkanApplicationTutorial18.InitVulkan;
begin
  inherited InitVulkan;
  CreateDescriptorPool;
  CreateDescriptorSets;
end;

procedure TVulkanApplicationTutorial18.Cleanup;
begin
  with getVulkan do
    vkDestroyDescriptorPool(FDevice, FDescriptorPool, nil);
  inherited Cleanup;
end;

end.
