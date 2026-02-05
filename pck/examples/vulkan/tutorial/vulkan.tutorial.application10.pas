unit vulkan.tutorial.application10;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  vulkan.tutorial.application09,
  pax.vulkan,
  pax.vulkan.helpers,
  vulkan.tutorial;

type
  { TVulkanApplicationTutorial10 }

  TVulkanApplicationTutorial10 = class(TVulkanApplicationTutorial09, IVulkanTutorial)
  protected
    procedure InitVulkan; override;
    procedure Cleanup; override;
    function ReadFileToBytes(const filename: string): TBytes;
    function CreateShaderModule(const code: TBytes): VkShaderModule;
  public
    constructor Create; override;
  end;

implementation

{ TVulkanApplicationTutorial10 }

constructor TVulkanApplicationTutorial10.Create;
begin
  inherited Create;
  Caption := 'Vulkan Tutorial 10 - Shader Modules';
end;

function TVulkanApplicationTutorial10.ReadFileToBytes(const filename: string): TBytes;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(filename, fmOpenRead);
  try
    SetLength(Result, fs.Size);
    fs.ReadBuffer(Result[0], fs.Size);
  finally
    fs.Free;
  end;
end;

function TVulkanApplicationTutorial10.CreateShaderModule(const code: TBytes): VkShaderModule;
var
  createInfo: VkShaderModuleCreateInfo;
begin
  createInfo.zero;
  createInfo.sType := VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
  createInfo.codeSize := Length(code);
  createInfo.pCode := @code[0];

  with getVulkan do
    if vkCreateShaderModule(FDevice, @createInfo, nil, Result) <> VK_SUCCESS then
      raise Exception.Create('Impossibile creare shader module');
end;

procedure TVulkanApplicationTutorial10.InitVulkan;
var
  vertCode, fragCode: TBytes;
  vertModule, fragModule: VkShaderModule;
begin
  inherited InitVulkan;
  vertCode := ReadFileToBytes('shaders/vert.spv');
  fragCode := ReadFileToBytes('shaders/frag.spv');
  vertModule := CreateShaderModule(vertCode);
  fragModule := CreateShaderModule(fragCode);
  // Usali in pipeline nel tutorial successivo
  with getVulkan do
  begin
    vkDestroyShaderModule(FDevice, fragModule, nil);
    vkDestroyShaderModule(FDevice, vertModule, nil);
  end;
end;

procedure TVulkanApplicationTutorial10.Cleanup;
begin
  inherited Cleanup;
end;

end.
