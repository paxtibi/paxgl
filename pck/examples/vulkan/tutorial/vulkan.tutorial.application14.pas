unit vulkan.tutorial.application14;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pax.vulkan,
  pax.vulkan.helpers,
  vulkan.tutorial,
  vulkan.tutorial.application13;

type
  { TVulkanApplicationTutorial14 }

  TVulkanApplicationTutorial14 = class(TVulkanApplicationTutorial13, IVulkanTutorial)
  protected
    FVertexBuffer: VkBuffer;
    FVertexBufferMemory: VkDeviceMemory;
  protected
    procedure InitVulkan; override;
    procedure Cleanup; override;
    procedure CreateVertexBuffer;
    function FindMemoryType(typeFilter: uint32; properties: VkMemoryPropertyFlags): uint32;
    procedure RecordCommandBuffer(commandBuffer: VkCommandBuffer; imageIndex: uint32);
  public
    constructor Create; override;
  end;

implementation

{ TVulkanApplicationTutorial14 }

constructor TVulkanApplicationTutorial14.Create;
begin
  inherited Create;
  FVertexBuffer := VK_NULL_HANDLE;
  FVertexBufferMemory := VK_NULL_HANDLE;
  Caption := 'Vulkan Tutorial 14 - Vertex Buffer Creation';
end;

function TVulkanApplicationTutorial14.FindMemoryType(typeFilter: uint32; properties: VkMemoryPropertyFlags): uint32;
var
  memProperties: VkPhysicalDeviceMemoryProperties;
  i: integer;
begin
  with getVulkan do
  begin
    vkGetPhysicalDeviceMemoryProperties(FPhysicalDevice, @memProperties);
    for i := 0 to memProperties.memoryTypeCount - 1 do
      if ((typeFilter and (1 shl i)) <> 0) and ((memProperties.memoryTypes[i].propertyFlags and properties) = properties) then
        Exit(i);
  end;
  raise Exception.Create('Impossibile trovare tipo di memoria adatto');
end;

procedure TVulkanApplicationTutorial14.CreateVertexBuffer;
var
  vertices: array[0..2] of TVertex;
  bufferInfo: VkBufferCreateInfo;
  memRequirements: VkMemoryRequirements;
  allocInfo: VkMemoryAllocateInfo;
  Data: Pointer;
begin
  // Definizione completa dei vertici (triangolo colorato)
  vertices[0].pos := TVec2.Create(-0.5, -0.5);
  vertices[0].color := TVec3.Create(1.0, 0.0, 0.0);
  vertices[1].pos := TVec2.Create(0.5, -0.5);
  vertices[1].color := TVec3.Create(0.0, 1.0, 0.0);
  vertices[2].pos := TVec2.Create(0.0, 0.5);
  vertices[2].color := TVec3.Create(0.0, 0.0, 1.0);

  bufferInfo.zero;
  bufferInfo.sType := VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
  bufferInfo.size := SizeOf(vertices);
  bufferInfo.usage := VK_BUFFER_USAGE_VERTEX_BUFFER_BIT;
  bufferInfo.sharingMode := VK_SHARING_MODE_EXCLUSIVE;

  with getVulkan do
    if vkCreateBuffer(FDevice, @bufferInfo, nil, @FVertexBuffer) <> VK_SUCCESS then
      raise Exception.Create('Impossibile creare vertex buffer');
  with getVulkan do
  begin
    vkGetBufferMemoryRequirements(FDevice, FVertexBuffer, @memRequirements);

    allocInfo.zero;
    allocInfo.sType := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
    allocInfo.allocationSize := memRequirements.size;
    allocInfo.memoryTypeIndex := FindMemoryType(memRequirements.memoryTypeBits, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT or VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);

    if vkAllocateMemory(FDevice, @allocInfo, nil, @FVertexBufferMemory) <> VK_SUCCESS then
      raise Exception.Create('Impossibile allocare memoria vertex buffer');

    vkBindBufferMemory(FDevice, FVertexBuffer, FVertexBufferMemory, 0);

    vkMapMemory(FDevice, FVertexBufferMemory, 0, bufferInfo.size, 0, @Data);
    Move(vertices[0], Data^, bufferInfo.size);
    vkUnmapMemory(FDevice, FVertexBufferMemory);
  end;
end;

procedure TVulkanApplicationTutorial14.RecordCommandBuffer(commandBuffer: VkCommandBuffer; imageIndex: uint32);
var
  buffers: array[0..0] of VkBuffer;
  offsets: array[0..0] of VkDeviceSize;
begin
  inherited RecordCommandBuffer(commandBuffer, imageIndex);
  with getVulkan do
  begin
    buffers[0] := FVertexBuffer;
    offsets[0] := 0;
    vkCmdBindVertexBuffers(commandBuffer, 0, 1, @buffers[0], @offsets[0]);
    vkCmdDraw(commandBuffer, 3, 1, 0, 0);  // 3 vertici per il triangolo
  end;
end;

procedure TVulkanApplicationTutorial14.InitVulkan;
begin
  inherited InitVulkan;
  CreateVertexBuffer;
end;

procedure TVulkanApplicationTutorial14.Cleanup;
begin
  with getVulkan do
  begin
    vkDestroyBuffer(FDevice, FVertexBuffer, nil);
    vkFreeMemory(FDevice, FVertexBufferMemory, nil);
  end;
  inherited Cleanup;
end;

end.
