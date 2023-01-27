{ lib
, stdenv
, python3
  # , buildPythonPackage
  # , fetchPypi
, libffi

, darwin
, xcbuild
, xcbuildHook

  # Darwin dependencies
, frameworks
}:

python3.pkgs.buildPythonPackage rec {
  pname = "pyobjc-core";
  version = "9.0.1";

  # See the guide for more information: https://nixos.org/nixpkgs/manual/#chap-pkgs-fetchers
  src = python3.pkgs.fetchPypi {
    inherit pname version;
    sha256 = "sha256-XOFRC7C9/1J8WXB5pCsuE6GbdZLnaFC+eWCid1tZySk=";
  };

  # Darwin stdenv unsets SDKROOT, so we can't set it as an attr
  preConfigure = ''
    # export SDKROOT="${darwin.apple_sdk_11_0.MacOSX-SDK}"

    # ln -s ${darwin.apple_sdk_11_0.MacOSX-SDK} MacOSX-11.0.0.sdk
    # export SDKROOT=./MacOSX-11.0.0.sdk

    # Otherwise Nix's libffi headers can't be found
    substituteInPlace Modules/objc/selector.h \
                      Modules/objc/libffi_extra.h \
                      Modules/objc/libffi_support.h \
                      Modules/objc/libffi_extra.m \
                      --replace "#include <ffi/ffi.h>" "#include <ffi.h>"
  '';

  # List of flags passed to `setup.py build_ext` command.
  setupPyBuildFlags = [ "--no-warnings-as-errors" "--sdk-root=${darwin.apple_sdk_11_0.MacOSX-SDK}" ];

  # See the guide for more information: https://nixos.org/nixpkgs/manual/#ssec-stdenv-dependencies
  propagatedBuildInputs = [ ];

  # NIX_DEBUG = true;

  enableParallelBuilding = true;

  makeFlags = [ "SDKROOT=${darwin.apple_sdk.MacOSX-SDK}" ];
  buildInputs = with frameworks; [
    Foundation
    CoreFoundation
    CoreServices
    AVFoundation
    Cocoa
    GameplayKit
    SpriteKit
    SceneKit
    GLKit
    MetalPerformanceShaders

    # AGL
    # AVFoundation
    # AVKit
    # Accelerate
    # Accessibility
    # Accounts
    # AdSupport
    # AddressBook
    # AppKit
    # AppTrackingTransparency
    # AppleScriptKit
    # AppleScriptObjC
    # ApplicationServices
    # AudioToolbox
    # AudioUnit
    # AudioVideoBridging
    # AuthenticationServices
    # AutomaticAssessmentConfiguration
    # Automator
    # BackgroundTasks
    # BusinessChat
    # CFNetwork
    # CalendarStore
    # CallKit
    # Carbon
    # ClassKit
    # CloudKit
    # Cocoa
    # Collaboration
    # ColorSync
    # Combine
    # Contacts
    # ContactsUI
    # CoreAudio
    # CoreAudioKit
    # CoreAudioTypes
    # CoreBluetooth
    # CoreData
    # CoreDisplay
    # CoreFoundation
    # CoreGraphics
    # CoreHaptics
    # CoreImage
    # CoreLocation
    # CoreMIDI
    # CoreMIDIServer
    # CoreML
    # CoreMedia
    # CoreMediaIO
    # CoreMotion
    # CoreServices
    # CoreSpotlight
    # CoreTelephony
    # CoreText
    # CoreVideo
    # CoreWLAN
    # CryptoKit
    # CryptoTokenKit
    # DVDPlayback
    # DeveloperToolsSupport
    # DeviceCheck
    # DirectoryService
    # DiscRecording
    # DiscRecordingUI
    # DiskArbitration
    # DriverKit
    # EventKit
    # ExceptionHandling
    # ExecutionPolicy
    # ExternalAccessory
    # FWAUserLib
    # FileProvider
    # FileProviderUI
    # FinderSync
    # ForceFeedback
    # Foundation
    # GLKit
    # GLUT
    # GSS
    # GameController
    # GameKit
    # GameplayKit
    # HIDDriverKit
    # Hypervisor
    # ICADevices
    # IMServicePlugIn
    # IOBluetooth
    # IOBluetoothUI
    # IOKit
    # IOSurface
    # IOUSBHost
    # IdentityLookup
    # ImageCaptureCore
    # ImageIO
    # InputMethodKit
    # InstallerPlugins
    # InstantMessage
    # Intents
    # JavaNativeFoundation
    # JavaRuntimeSupport
    # JavaScriptCore
    # Kerberos
    # Kernel
    # KernelManagement
    # LDAP
    # LatentSemanticMapping
    # LinkPresentation
    # LocalAuthentication
    # MLCompute
    # MapKit
    # MediaAccessibility
    # MediaLibrary
    # MediaPlayer
    # MediaToolbox
    # Message
    # Metal
    # MetalKit
    # MetalPerformanceShaders
    # MetalPerformanceShadersGraph
    # MetricKit
    ModelIO
    # MultipeerConnectivity
    # NaturalLanguage
    # NearbyInteraction
    # NetFS
    # Network
    # NetworkExtension
    # NetworkingDriverKit
    # NotificationCenter
    # OSAKit
    # OSLog
    # OpenAL
    # OpenCL
    # OpenDirectory
    # OpenGL
    # PCIDriverKit
    # PCSC
    # PDFKit
    # ParavirtualizedGraphics
    # PassKit
    # PencilKit
    # Photos
    # PhotosUI
    # PreferencePanes
    # PushKit
    # Python
    # QTKit
    # Quartz
    # QuartzCore
    # QuickLook
    # QuickLookThumbnailing
    # RealityKit
    # ReplayKit
    # Ruby
    # SafariServices
    # SceneKit
    # ScreenSaver
    # ScreenTime
    # ScriptingBridge
    # Security
    # SecurityFoundation
    # SecurityInterface
    # SensorKit
    # ServiceManagement
    # Social
    # SoundAnalysis
    # Speech
    # SpriteKit
    # StoreKit
    # SwiftUI
    # SyncServices
    # System
    # SystemConfiguration
    # SystemExtensions
    # TWAIN
    # Tcl
    # Tk
    # USBDriverKit
    # UniformTypeIdentifiers
    # UserNotifications
    # UserNotificationsUI
    # VideoDecodeAcceleration
    # VideoSubscriberAccount
    # VideoToolbox
    # Virtualization
    # Vision
    # WebKit
    # WidgetKit
    # iTunesLibrary
    # vmnet
  ] ++ [
    darwin.cctools
    libffi
  ];

  # pythonImportsCheck = [ "objc._objc" ];

  # See the guide for more information: https://nixos.org/nixpkgs/manual/#chap-meta
  meta = with lib; {
    description = "Python<->ObjC Interoperability Module";
    homepage = "https://github.com/ronaldoussoren/pyobjc";
    license = licenses.mit;
    maintainers = with maintainers; [ cfeeley ];
  };
}
