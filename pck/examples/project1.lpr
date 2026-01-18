program VulkanGLFWExample;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  glfw3,
  pax.vulkan;

var
  window: PGLFWwindow;
  instance: VkInstance;
  createInfo: VkInstanceCreateInfo;
  appInfo: VkApplicationInfo;
  result: VkResult;

procedure ErrorCallback(err: integer; const description: PAnsiChar); cdecl;
begin
  WriteLn('GLFW Error ', err, ': ', description);
end;

function CheckVulkanSupport: boolean;
var
  glfwExtensions: PPAnsiChar;
  glfwExtensionCount: uint32;
  i: Integer;
begin
  Result := false;

  if glfwVulkanSupported() = GLFW_FALSE then
  begin
    WriteLn('GLFW: Vulkan non supportato!');
    Exit;
  end;

  glfwExtensionCount := glfwGetRequiredInstanceExtensions(@glfwExtensions);
  if glfwExtensionCount = 0 then
  begin
    WriteLn('GLFW: Nessuna estensione richiesta restituita');
    Exit;
  end;

  WriteLn('Estensioni richieste da GLFW (', glfwExtensionCount, '):');
  for i := 0 to glfwExtensionCount - 1 do
    WriteLn('  ', glfwExtensions[i]);

  Result := true;
end;

begin
  // Inizializzazione GLFW
  if glfwInit = GLFW_FALSE then
  begin
    WriteLn('Errore inizializzazione GLFW');
    Exit;
  end;

  glfwSetErrorCallback(@ErrorCallback);

  glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
  glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);

  window := glfwCreateWindow(1280, 720, 'Vulkan + GLFW - Esempio base', nil, nil);
  if window = nil then
  begin
    glfwTerminate;
    WriteLn('Impossibile creare finestra GLFW');
    Exit;
  end;

  WriteLn('Finestra GLFW creata con successo');

  // Verifica supporto Vulkan tramite GLFW
  if not CheckVulkanSupport then
  begin
    glfwDestroyWindow(window);
    glfwTerminate;
    Exit;
  end;

  // ==============================================
  // Creazione istanza Vulkan molto basilare
  // ==============================================

  FillChar(appInfo, SizeOf(appInfo), 0);
  appInfo.sType := VK_STRUCTURE_TYPE_APPLICATION_INFO;
  appInfo.pApplicationName := 'Esempio GLFW+Vulkan';
  appInfo.applicationVersion := VK_MAKE_VERSION(1, 0, 0);
  appInfo.pEngineName := 'Nessun engine';
  appInfo.engineVersion := VK_MAKE_VERSION(1, 0, 0);
  appInfo.apiVersion := VK_API_VERSION_1_0;

  FillChar(createInfo, SizeOf(createInfo), 0);
  createInfo.sType := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
  createInfo.pApplicationInfo := @appInfo;

  // Estensioni richieste da GLFW
  createInfo.enabledExtensionCount := glfwGetRequiredInstanceExtensions(@createInfo.ppEnabledExtensionNames);
  createInfo.ppEnabledExtensionNames := glfwGetRequiredInstanceExtensions(@createInfo.enabledExtensionCount);

  // Layers (esempio: validation layer - commentare in release)
  // createInfo.enabledLayerCount := 1;
  // createInfo.ppEnabledLayerNames := @VALIDATION_LAYER_NAME;

  result := vkCreateInstance(@createInfo, nil, @instance);
  if result <> VK_SUCCESS then
  begin
    WriteLn('Errore creazione istanza Vulkan: ', Ord(result));
    glfwDestroyWindow(window);
    glfwTerminate;
    Exit;
  end;

  WriteLn('Istanza Vulkan creata con successo!');

  // ==============================================
  // Ciclo principale
  // ==============================================

  while glfwWindowShouldClose(window) = GLFW_FALSE do
  begin
    glfwPollEvents;
  end;

  // Cleanup
  vkDestroyInstance(instance, nil);
  glfwDestroyWindow(window);
  glfwTerminate;

  WriteLn('Applicazione terminata correttamente.');
end.
