unit vulkan.tutorial.application16;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pax.vulkan,
  pax.vulkan.helpers,
  vulkan.tutorial,
  vulkan.tutorial.application15;

type
  { TVulkanApplicationTutorial16 }

  TVulkanApplicationTutorial16 = class(TVulkanApplicationTutorial15, IVulkanTutorial)
  protected
    FIndexBuffer: VkBuffer;
    FIndexBufferMemory: VkDeviceMemory;
  protected
    procedure InitVulkan; override;
    procedure Cleanup; override;
    procedure CreateIndexBuffer;
    procedure RecordCommandBuffer(commandBuffer: VkCommandBuffer; imageIndex: uint32);
  public
    constructor Create; override;
  end;

implementation

{ TVulkanApplicationTutorial16 }

constructor TVulkanApplicationTutorial16.Create;
begin
  inherited Create;
  FIndexBuffer := VK_NULL_HANDLE;
  FIndexBufferMemory := VK_NULL_HANDLE;
  Caption := 'Vulkan Tutorial 16 - Index Buffer';
end;

procedure TVulkanApplicationTutorial16.CreateIndexBuffer;
var
  indices: array[0..5] of uint32;  // Esempio per un quadrato (2 triangoli)
  bufferSize: VkDeviceSize;
  stagingBuffer: VkBuffer;
  stagingBufferMemory: VkDeviceMemory;
  Data: Pointer;
begin
  // Definizione completa degli indici
  indices[0] := 0;
  indices[1] := 1;
  indices[2] := 2;
  indices[3] := 2;
  indices[4] := 3;
  indices[5] := 0;  // Assumendo 4 vertici per quadrato

  bufferSize := SizeOf(indices);

  CreateBuffer(bufferSize, VK_BUFFER_USAGE_TRANSFER_SRC_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT or VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, stagingBuffer, stagingBufferMemory);

  with getVulkan do
  begin
    vkMapMemory(FDevice, stagingBufferMemory, 0, bufferSize, 0, @Data);
    Move(indices[0], Data^, bufferSize);
    vkUnmapMemory(FDevice, stagingBufferMemory);
  end;

  CreateBuffer(bufferSize, VK_BUFFER_USAGE_TRANSFER_DST_BIT or VK_BUFFER_USAGE_INDEX_BUFFER_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, FIndexBuffer, FIndexBufferMemory);

  CopyBuffer(stagingBuffer, FIndexBuffer, bufferSize);

  with getVulkan do
  begin
    vkDestroyBuffer(FDevice, stagingBuffer, nil);
    vkFreeMemory(FDevice, stagingBufferMemory, nil);
  end;
end;

procedure TVulkanApplicationTutorial16.RecordCommandBuffer(commandBuffer: VkCommandBuffer; imageIndex: uint32);
begin
  inherited RecordCommandBuffer(commandBuffer, imageIndex);
  with getVulkan do
  begin
    vkCmdBindIndexBuffer(commandBuffer, FIndexBuffer, 0, VK_INDEX_TYPE_UINT32);
    vkCmdDrawIndexed(commandBuffer, 6, 1, 0, 0, 0);  // 6 indici per 2 triangoli
  end;
end;

procedure TVulkanApplicationTutorial16.InitVulkan;
begin
  inherited InitVulkan;
  CreateIndexBuffer;
end;

procedure TVulkanApplicationTutorial16.Cleanup;
begin
  with getVulkan do
  begin
    vkDestroyBuffer(FDevice, FIndexBuffer, nil);
    vkFreeMemory(FDevice, FIndexBufferMemory, nil);
  end;
  inherited Cleanup;
end;

end.
