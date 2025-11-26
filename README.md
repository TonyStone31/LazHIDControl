# LazHIDControl - Cross-Platform HID Automation

**LazHIDControl** extends the foundational work of **Tom Gregorovic's LazMouseAndKeyInput package**, adding modern platform support and global hotkey functionality. This package enables precise mouse and keyboard automation with cross-platform global hotkey registration, making it a powerful tool for building **xdotool replacements** and HID automation tools using **Free Pascal**.

---

## 🎯 Project Goals
- **Enhance Cross-Platform Support**:
  - Add and refine **Wayland** support, alongside existing support for **X Windows** and **Microsoft Windows**.
  - Continue improving and testing **macOS** compatibility (community contributions welcome!).
- **Provide a Free Pascal Alternative to xdotool**:
  - Enable precise mouse and keyboard automation across platforms.
  - Deliver a lightweight, open-source, and developer-friendly tool.

---

## 🚀 Quick Start

### Installation
1. In Lazarus IDE: **Package → Open Package File (.lpk)** → Select `lazhidcontrol/lazhidcontrol.lpk`
2. Click **Compile** then **Use → Add to Project**

### Basic Usage
```pascal
uses
  LazHIDControl;

// Simulate typing
KeyInput.PressString('Hello World!');

// Move and click mouse
MouseInput.Move([], 100, 100, 500);
MouseInput.Click(mbLeft, []);

// Register global hotkey (Ctrl+Shift+F9)
var
  MyHotkey: THotkey;
begin
  MyHotkey := THotkey.Create(VK_F9, [ssCtrl, ssShift], @OnHotkeyHandler);
  MyHotkey.Register;
end;
```

See `USAGE_EXAMPLES.md` for complete examples and `HOTKEY_USAGE.md` for hotkey documentation.

---

## 🛠 Current Status
### Platforms
- **Linux (X Windows)**:
  - Mouse and keyboard input functionality is stable and reliable.
  - Global hotkey registration fully supported via XGrabKey.
- **Microsoft Windows**:
  - Most features have been successfully tested and are performing as expected.
  - Global hotkey registration fully supported via low-level keyboard hooks.
- **Wayland**:
  - Basic functionality is operational, but:
    - **Requires elevated permissions** or proper user access to input devices.
    - Advanced features are still under exploration.
  - Global hotkey registration supported on GNOME 45+ and KDE Plasma 5/6 via portal API.
- **macOS**:
  - Currently untested due to lack of access to Mac systems.

---

## 🗓 Future Plans
- **Wayland**:
  - Resolve permission-related issues for easier usability.
  - Explore compositor-specific protocols for advanced functionality.
- **macOS**:
  - Enable macOS support through testing and contributions.
- **Additional Features**:
  - Fully replicate and enhance **xdotool** capabilities.
  - Ensure seamless operation across all supported platforms.

---

## 🤝 Contributions
Contributions are welcome! If you have experience with **Wayland**, **macOS**, or advanced input automation, your expertise is invaluable.

- Fork the repository and submit a pull request.
- Report issues or suggest features via [GitHub Issues](https://github.com/TonyStone31/LazHIDControl/issues).

---

## 📜 Acknowledgments
- **Tom Gregorovic** - Original author of LazMouseAndKeyInput package and Carbon (macOS) implementation
- **Sammarco Francesco** - Cocoa (macOS) implementation
- **Codebot** - Cross-platform hotkey implementation adapted from the Codebot Pascal Library
- **Tony Stone** - Extended package with Wayland support, global hotkey registration, and modern platform enhancements

---

## 🔄 Why a New Package?

This project extends the original **[LazMouseAndKeyInput](https://gitlab.com/freepascal.org/lazarus/lazarus/-/tree/main/components/mouseandkeyinput?ref_type=heads)** package rather than contributing patches back for several practical reasons:

- **Package Conflicts**: During development with multiple Lazarus installations, conflicts arose between the built-in package and extended versions
- **Cleaner Extension Path**: Starting fresh allowed for significant feature additions (global hotkeys, Wayland support) without compatibility concerns

This approach enables rapid development and experimentation while maintaining compatibility with the original package's design philosophy.

---

## 📚 Additional Resources
- [Free Pascal](https://www.freepascal.org/)
- [Lazarus IDE](https://www.lazarus-ide.org/)
- [xdotool](https://github.com/jordansissel/xdotool)
