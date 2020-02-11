#include <windows.h>
#include <libloaderapi.h>
#include <vector>
#include <iostream>
#include <csignal>
#include <plugin.hpp>
#include <Plugin.hpp>

//#include <osdialog.h>
//#include <app.hpp>
//#include <asset.hpp>
//#include <logger.hpp>

extern "C" long long hs_GetDataPointer(rack::plugin::Plugin* plugin);
long long hs_GetDataPointer(rack::plugin::Plugin* plugin){
  std::cout << '[';
  for (auto&& i : plugin->models) std::cout << 'x';
  std::cout << ']';
  long long x = (long long) (plugin -> models.data());
  return x;
}


typedef void (*InitCallback)(rack::plugin::Plugin*);

extern "C" rack::plugin::Plugin* hs_LoadRackPlugin(const char *plugin);
rack::plugin::Plugin* hs_LoadRackPlugin(const char *plugin){
  typedef DLL_DIRECTORY_COOKIE(WINAPI * PAddDllDirectory)(PCWSTR);
  typedef BOOL(WINAPI * PSetDefaultDllDirectories)(DWORD);

  PAddDllDirectory add_dll_directory = (PAddDllDirectory)GetProcAddress(GetModuleHandle("kernel32.dll"), "AddDllDirectory");
  PSetDefaultDllDirectories set_default_dll_directories = (PSetDefaultDllDirectories)GetProcAddress(GetModuleHandle("kernel32.dll"), "SetDefaultDllDirectories");

  //std::raise(SIGINT);
  SetErrorMode(0x8001);
  set_default_dll_directories(0x1700);
  //add_dll_directory(L"C:\\ProgramFiles\\VCV\\Rack");
  add_dll_directory(L"C:\\Users\\Sam\\Documents\\GitHub\\boopadoop\\cbits\\");
  //add_dll_directory(L"C:\\Users\\Sam\\Documents\\GitHub\\boopadoop\\plugin\\");
  add_dll_directory(L"C:\\Users\\Sam\\Documents\\GitHub\\boopadoop\\");
  HINSTANCE handle = LoadLibrary("C:\\Users\\Sam\\Documents\\GitHub\\boopadoop\\plugin.dll");
  SetErrorMode(0);
  if (!handle) {
    int error = GetLastError();
    std::cout << "Error loading plugin: " << error;
  }
  rack::plugin::Plugin* thePlugin = new rack::plugin::Plugin;
  //memset((void*)plugin,0xdead,1024);
  thePlugin->handle = handle;
  thePlugin->path = "plugin";

  return thePlugin;
}
extern "C" InitCallback hs_GetInitCallback(rack::plugin::Plugin* plugin);
InitCallback hs_GetInitCallback(rack::plugin::Plugin* plugin){
  std::cout << "Plugin path is: " << plugin->path;

  //rack::plugin::Model* fakeModel = new rack::plugin::Model;
  //fakeModel->plugin = plugin;
  //plugin->models.push_back(fakeModel);
  plugin->models.push_back(NULL);
  std::cout << "data pointer is: " << (long long) plugin->models.data() << "\n";
  
  InitCallback initCallback = (InitCallback) GetProcAddress((HINSTANCE)(plugin->handle),"init");
  if (!initCallback) {
    std::cout << "Null init callback!\n";
  }
  //initCallback(plugin);
  //rack::plugin::init();
  return initCallback;
}

/*
extern "C" rack::plugin::Model* hs_GetModelBySlug(rack::plugin::Plugin* plugin, const char *slug);
rack::plugin::Model* hs_GetModelBySlug(rack::plugin::Plugin* plugin, const char *slug){
  return plugin->getModel(slug);
}
*/


/*
int main(){

  // Windows global mutex to prevent running at the same time as actual Rack
  // Handle will be closed by Windows when the process ends
  HANDLE instanceMutex = CreateMutexA(NULL, true, "VCV Rack");
  if (GetLastError() == ERROR_ALREADY_EXISTS) {
    osdialog_message(OSDIALOG_ERROR, OSDIALOG_OK, "Rack is already running. Can't use Boopadoop rack support and Rack at the same time.");
    exit(1);
  }
  (void) instanceMutex;

  std::cout << "hi\n";
  rack::plugin::Plugin* plugin = hs_LoadRackPlugin("plugin/plugin.dll");
  std::cout << "still alive1\n";
  InitCallback cb = hs_GetInitCallback(plugin);
  std::cout << "still alive5\n";

  rack::asset::init();
  std::cout << "still alive18\n";
  rack::logger::init(); 
  std::cout << "still alive14\n";

  rack::plugin::init();


  //cb(plugin);
  std::cout << "still alive2\n";
}
*/
