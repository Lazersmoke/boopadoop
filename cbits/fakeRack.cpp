// g++ .\cbits\fakeRack.cpp -IRack-SDK\include\ -IRack-SDK\dep\include\ -lRack -LRack-SDK\ -o Rack -mwindows "-Wl,--export-all-symbols"

#include <jansson.h>
#include <iostream>
#include <plugin/Plugin.hpp>
#include <plugin/Model.hpp>
#include <plugin.hpp>
#include <string.hpp>
#include <app/common.hpp>

#include <ui/Menu.hpp>
#include <app/PortWidget.hpp>
#include <app/Scene.hpp>
#include <window.hpp>
#include <app.hpp>
#include <history.hpp>
#include <componentlibrary.hpp>
#include <app/LightWidget.hpp>
#include <color.hpp>

#include <app/ModuleWidget.hpp>
#include <app/Scene.hpp>
#include <engine/Engine.hpp>
#include <plugin/Plugin.hpp>
#include <app/SvgPanel.hpp>
#include <system.hpp>
#include <asset.hpp>
#include <helpers.hpp>
#include <app.hpp>
#include <settings.hpp>
#include <history.hpp>

#include <osdialog.h>
#include <thread>

#include <app/ParamWidget.hpp>
#include <ui/MenuOverlay.hpp>
#include <ui/TextField.hpp>
#include <app/Scene.hpp>
#include <app.hpp>
#include <engine/Engine.hpp>
#include <settings.hpp>
#include <history.hpp>
#include <helpers.hpp>

#include <ui/MenuItem.hpp>
#include <ui/MenuOverlay.hpp>

#include <app/MultiLightWidget.hpp>
#include <color.hpp>

#include <app/Knob.hpp>
#include <app.hpp>
#include <app/Scene.hpp>
#include <random.hpp>
#include <history.hpp>

#include <app/Switch.hpp>
#include <app.hpp>
#include <app/Scene.hpp>
#include <random.hpp>
#include <history.hpp>

#include <app/SvgKnob.hpp>
#include <app/SvgPort.hpp>
#include <app/SvgSlider.hpp>
#include <app/SvgScrew.hpp>
#include <app/ModuleLightWidget.hpp>


#include <common.hpp>
#include <math.hpp>
#include <string.hpp>
#include <system.hpp>
#include <random.hpp>
#include <network.hpp>
#include <asset.hpp>
#include <window.hpp>
#include <app.hpp>
#include <midi.hpp>
#include <helpers.hpp>
#include <componentlibrary.hpp>

namespace rack {
namespace plugin {

Plugin::~Plugin() {
  for (Model* model : models) {
    delete model;
  }
}

void Plugin::addModel(Model* model) {
  std::cout << "Unhandled!";
}

Model* Plugin::getModel(std::string slug) {}

void Plugin::fromJson(json_t* rootJ) {}

}


namespace ui {


#define BND_LABEL_FONT_SIZE 13

void MenuItem::draw(const DrawArgs& args) {}

void MenuItem::step() {}

void MenuItem::onEnter(const event::Enter& e) {}

void MenuItem::onDragDrop(const event::DragDrop& e) {}

void MenuItem::doAction() {}


Menu::Menu() {}
Menu::~Menu() {}
void Menu::setChildMenu(Menu* menu) {}
void Menu::step() {}
void Menu::draw(const DrawArgs& args) {}
void Menu::onHoverScroll(const event::HoverScroll& e) {}
MenuEntry::MenuEntry() {}
}


namespace app {

void MultiLightWidget::addBaseColor(NVGcolor baseColor) {}

void MultiLightWidget::setBrightnesses(const std::vector<float>& brightnesses) {}

struct PlugLight : MultiLightWidget {
  PlugLight() {
    addBaseColor(componentlibrary::SCHEME_GREEN);
    addBaseColor(componentlibrary::SCHEME_RED);
    addBaseColor(componentlibrary::SCHEME_BLUE);
    box.size = math::Vec(8, 8);
    bgColor = componentlibrary::SCHEME_BLACK_TRANSPARENT;
  }
};


PortWidget::PortWidget() {}

PortWidget::~PortWidget() {}

void PortWidget::step() {}
void PortWidget::draw(const DrawArgs& args) {}
void PortWidget::onButton(const event::Button& e) {}
void PortWidget::onEnter(const event::Enter& e) {}
void PortWidget::onLeave(const event::Leave& e) {}

void PortWidget::onDragStart(const event::DragStart& e) {}
void PortWidget::onDragEnd(const event::DragEnd& e) {}

void PortWidget::onDragDrop(const event::DragDrop& e) {}

void PortWidget::onDragEnter(const event::DragEnter& e) {}

void PortWidget::onDragLeave(const event::DragLeave& e) {}





void LightWidget::draw(const DrawArgs& args) {}

void LightWidget::drawLight(const DrawArgs& args) {}

void LightWidget::drawHalo(const DrawArgs& args) {}




static const char PRESET_FILTERS[] = "VCV Rack module preset (.vcvm):vcvm";

struct ModuleUrlItem : ui::MenuItem {
  std::string url;
  void onAction(const event::Action& e) override {}
};


struct ModuleFolderItem : ui::MenuItem {
  std::string path;
  void onAction(const event::Action& e) override {}
};


struct ModulePluginItem : ui::MenuItem {
  plugin::Plugin* plugin;
  ui::Menu* createChildMenu() override {}
};


struct ModuleDisconnectItem : ui::MenuItem {
  ModuleWidget* moduleWidget;
  void onAction(const event::Action& e) override {}
};


struct ModuleResetItem : ui::MenuItem {
  ModuleWidget* moduleWidget;
  void onAction(const event::Action& e) override {}
};


struct ModuleRandomizeItem : ui::MenuItem {
  ModuleWidget* moduleWidget;
  void onAction(const event::Action& e) override {}
};


struct ModuleCopyItem : ui::MenuItem {
  ModuleWidget* moduleWidget;
  void onAction(const event::Action& e) override {}
};


struct ModulePasteItem : ui::MenuItem {
  ModuleWidget* moduleWidget;
  void onAction(const event::Action& e) override {}
};


struct ModuleSaveItem : ui::MenuItem {
  ModuleWidget* moduleWidget;
  void onAction(const event::Action& e) override {}
};


struct ModuleLoadItem : ui::MenuItem {
  ModuleWidget* moduleWidget;
  void onAction(const event::Action& e) override {}
};


struct ModulePresetPathItem : ui::MenuItem {
  ModuleWidget* moduleWidget;
  std::string presetPath;
  void onAction(const event::Action& e) override {}
};


struct ModulePresetItem : ui::MenuItem {
  ModuleWidget* moduleWidget;
  ui::Menu* createChildMenu() override {}
};


struct ModuleCloneItem : ui::MenuItem {
  ModuleWidget* moduleWidget;
  void onAction(const event::Action& e) override {}
};


struct ModuleBypassItem : ui::MenuItem {
  ModuleWidget* moduleWidget;
  void onAction(const event::Action& e) override {}
};


struct ModuleDeleteItem : ui::MenuItem {
  ModuleWidget* moduleWidget;
  void onAction(const event::Action& e) override {}
};


ModuleWidget::ModuleWidget() {}

ModuleWidget::~ModuleWidget() {}

void ModuleWidget::draw(const DrawArgs& args) {}

void ModuleWidget::drawShadow(const DrawArgs& args) {}

void ModuleWidget::onButton(const event::Button& e) {}

void ModuleWidget::onHoverKey(const event::HoverKey& e) {}

void ModuleWidget::onDragStart(const event::DragStart& e) {}

void ModuleWidget::onDragEnd(const event::DragEnd& e) {}

void ModuleWidget::onDragMove(const event::DragMove& e) {}

void ModuleWidget::setModule(engine::Module* module) {}

void ModuleWidget::setPanel(std::shared_ptr<Svg> svg) {}

void ModuleWidget::addParam(ParamWidget* param) {}

void ModuleWidget::addOutput(PortWidget* output) {}

void ModuleWidget::addInput(PortWidget* input) {}

ParamWidget* ModuleWidget::getParam(int paramId) {}

PortWidget* ModuleWidget::getOutput(int outputId) {}

PortWidget* ModuleWidget::getInput(int inputId) {}

json_t* ModuleWidget::toJson() {}

void ModuleWidget::fromJson(json_t* rootJ) {}

void ModuleWidget::copyClipboard() {}

void ModuleWidget::pasteClipboardAction() {}

void ModuleWidget::loadAction(std::string filename) {}

void ModuleWidget::save(std::string filename) {}

void ModuleWidget::loadDialog() {}

void ModuleWidget::saveDialog() {}

void ModuleWidget::disconnect() {}

void ModuleWidget::resetAction() {}

void ModuleWidget::randomizeAction() {}

static void disconnectActions(ModuleWidget* mw, history::ComplexAction* complexAction) {}

void ModuleWidget::disconnectAction() {}

void ModuleWidget::cloneAction() {}

void ModuleWidget::bypassAction() {}

void ModuleWidget::removeAction() {}

void ModuleWidget::createContextMenu() {}




struct ParamField : ui::TextField {
  ParamWidget* paramWidget;

  void step() override {}

  void setParamWidget(ParamWidget* paramWidget) {}

  void onSelectKey(const event::SelectKey& e) override {}
};


struct ParamTooltip : ui::Tooltip {
  ParamWidget* paramWidget;

  void step() override {}
};


struct ParamLabel : ui::MenuLabel {
  ParamWidget* paramWidget;
  void step() override {}
};


struct ParamResetItem : ui::MenuItem {
  ParamWidget* paramWidget;
  void onAction(const event::Action& e) override {}
};


struct ParamFineItem : ui::MenuItem {
};


struct ParamUnmapItem : ui::MenuItem {
  ParamWidget* paramWidget;
  void onAction(const event::Action& e) override {}
};


void ParamWidget::step() {}

void ParamWidget::draw(const DrawArgs& args) {}

void ParamWidget::onButton(const event::Button& e) {}

void ParamWidget::onDoubleClick(const event::DoubleClick& e) {}

void ParamWidget::onEnter(const event::Enter& e) {}

void ParamWidget::onLeave(const event::Leave& e) {}

void ParamWidget::fromJson(json_t* rootJ) {}

void ParamWidget::createContextMenu() {}

void ParamWidget::resetAction() {}

static const float KNOB_SENSITIVITY = 0.0015f;


void Knob::onHover(const event::Hover& e) {}

void Knob::onButton(const event::Button& e) {}

void Knob::onDragStart(const event::DragStart& e) {}

void Knob::onDragEnd(const event::DragEnd& e) {}

void Knob::onDragMove(const event::DragMove& e) {}

void Knob::reset() {}

void Knob::randomize() {}

void Switch::step() {}

void Switch::onDoubleClick(const event::DoubleClick& e) {}

void Switch::onDragStart(const event::DragStart& e) {}

void Switch::onDragEnd(const event::DragEnd& e) {}

void Switch::reset() {}

void Switch::randomize() {}

SvgKnob::SvgKnob() {}

void SvgKnob::setSvg(std::shared_ptr<Svg> svg) {}

void SvgKnob::onChange(const event::Change& e) {}

SvgPort::SvgPort() {}

void SvgPort::setSvg(std::shared_ptr<Svg> svg) {}

SvgSlider::SvgSlider() {}

void SvgSlider::setBackgroundSvg(std::shared_ptr<Svg> svg) {}

void SvgSlider::setHandleSvg(std::shared_ptr<Svg> svg) {}

void SvgSlider::onChange(const event::Change& e) {}

SvgScrew::SvgScrew() {}

void SvgScrew::setSvg(std::shared_ptr<Svg> svg) {}

SvgSwitch::SvgSwitch() {}

void SvgSwitch::addFrame(std::shared_ptr<Svg> svg) {}

void SvgSwitch::onChange(const event::Change& e) {}


void ModuleLightWidget::step() {}

}
namespace dsp {
void minBlepImpulse(int z, int o, float* output) {}
}

namespace asset {
std::string plugin(plugin::Plugin* plugin, std::string filename) {
  assert(plugin);
  return plugin->path + "/" + filename;
}

std::string system(std::string filename) {
  return systemDir + "/" + filename;
}
}

std::shared_ptr<Font> Window::loadFont(const std::string& filename) {}

std::shared_ptr<Image> Window::loadImage(const std::string& filename) {}

std::shared_ptr<Svg> Window::loadSvg(const std::string& filename) {}

App* appGet() {}

namespace engine {

engine::Param* ParamQuantity::getParam() {}

void ParamQuantity::setSmoothValue(float smoothValue) {}

float ParamQuantity::getSmoothValue() {}

void ParamQuantity::setValue(float value) {}

float ParamQuantity::getValue() {}

float ParamQuantity::getMinValue() {}

float ParamQuantity::getMaxValue() {}

float ParamQuantity::getDefaultValue() {}

float ParamQuantity::getDisplayValue() {}

void ParamQuantity::setDisplayValue(float displayValue) {}

int ParamQuantity::getDisplayPrecision() {}

std::string ParamQuantity::getDisplayValueString() {}

void ParamQuantity::setDisplayValueString(std::string s) {}

std::string ParamQuantity::getLabel() {}

std::string ParamQuantity::getUnit() {}

float Engine::getSampleRate() {}

float Engine::getSampleTime() {}

void Engine::yieldWorkers() {}

Module::Module() {}

Module::~Module() {}

void Module::config(int numParams, int numInputs, int numOutputs, int numLights) {}

}

namespace random {
float uniform() {}
}


namespace string {


std::string fromWstring(const std::wstring& s) {}


std::wstring toWstring(const std::string& s) {}


std::string f(const char* format, ...) {}


std::string lowercase(const std::string& s) {}


std::string uppercase(const std::string& s) {}


std::string trim(const std::string& s) {}


std::string ellipsize(const std::string& s, size_t len) {}


std::string ellipsizePrefix(const std::string& s, size_t len) {}


bool startsWith(const std::string& str, const std::string& prefix) {}


bool endsWith(const std::string& str, const std::string& suffix) {}


std::string directory(const std::string& path) {}


std::string filename(const std::string& path) {}


std::string filenameBase(const std::string& filename) {}


std::string filenameExtension(const std::string& filename) {}


std::string absolutePath(const std::string& path) {}


float fuzzyScore(const std::string& s, const std::string& query) {}


std::string toBase64(const uint8_t* data, size_t dataLen) {}


std::string toBase64(const std::vector<uint8_t>& data) {}


std::vector<uint8_t> fromBase64(const std::string& str) {}


std::vector<uint8_t> compress(const uint8_t* data, size_t dataLen) {}


std::vector<uint8_t> compress(const std::vector<uint8_t>& data) {}


void uncompress(const uint8_t* compressed, size_t compressedLen, uint8_t* data, size_t* dataLen) {}


void uncompress(const std::vector<uint8_t>& compressed, uint8_t* data, size_t* dataLen) {}


}


namespace widget {

Widget::~Widget() {}

void Widget::setPosition(math::Vec pos) {}

void Widget::setSize(math::Vec size) {}

void Widget::show() {}

void Widget::hide() {}

void Widget::requestDelete() {}

math::Rect Widget::getChildrenBoundingBox() {}

math::Vec Widget::getRelativeOffset(math::Vec v, Widget* relative) {}

math::Rect Widget::getViewport(math::Rect r) {}

void Widget::addChild(Widget* child) {}

void Widget::addChildBottom(Widget* child) {}

void Widget::removeChild(Widget* child) {}

void Widget::clearChildren() {}

void Widget::step() {}

void Widget::draw(const DrawArgs& args) {}


}
}
json_t* json_array(void) {}
int json_array_append_new(json_t* a,json_t* b) {}
json_t *json_array_get(const json_t *json, size_t index) {}
int json_array_insert_new(json_t *json, size_t index, json_t *value) {}
json_t* json_false() {}
json_t* json_integer(json_int_t value) {}
json_int_t json_integer_value(const json_t *json) {}
json_t* json_object() {}
json_t *json_object_get(const json_t *json, const char *key) {}
int json_object_set_new_nocheck(json_t *json, const char *key, json_t *value) {}
int json_object_set_new(json_t *json, const char *key, json_t *value) {}
json_t* json_true() {}

void nvgFillColor(NVGcontext* ctx, NVGcolor color) {}
void nvgFontFaceId(NVGcontext* ctx, int font) {}
void nvgFontSize(NVGcontext* ctx, float size) {}
NVGcolor nvgRGB(unsigned char r, unsigned char g, unsigned char b) {}
NVGcolor nvgRGBf(float r, float g, float b) {}
NVGcolor nvgRGBA(unsigned char r, unsigned char g, unsigned char b, unsigned char a) {}
NVGcolor nvgRGBAf(float r, float g, float b, float a) {}
float nvgText(NVGcontext* ctx, float x, float y, const char* string, const char* end) {}
void nvgTextLetterSpacing(NVGcontext* ctx, float spacing) {}


