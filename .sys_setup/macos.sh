#!/bin/bash

# close System Preferences, to prevent interference
osascript -e 'tell application "System Preferences" to quit'

# elevate privileges 
sudo -v

# keep priviliges elevated until script finishes
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &


###############################################################################
# General UI/UX                                                               #
###############################################################################

echo Enter computer/host name.
read hostname

# set computer name (as in System Preferences / Sharing)
sudo scutil --set ComputerName $hostname
sudo scutil --set HostName $hostname
sudo scutil --set LocalHostName $hostname
sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName -string $hostname

# scrollbar visibility
# `WhenScrolling`, `Automatic` and `Always`
defaults write NSGlobalDomain AppleShowScrollBars -string "WhenScrolling" 

# expand save panel by default
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true

# expand print panel by default
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true

# automatically quit printer app once the print jobs complete
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

# disable the “Are you sure you want to open this application?” dialog
defaults write com.apple.LaunchServices LSQuarantine -bool false

# remove duplicates in the “Open With” menu (also see `lscleanup` alias)
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user

# disable the crash reporter
defaults write com.apple.CrashReporter DialogType -string "none"

# restart automatically if the computer freezes
sudo systemsetup -setrestartfreeze on


###############################################################################
# Locale                                                                      #
###############################################################################

# set formats
defaults write NSGlobalDomain AppleMeasurementUnits -string "Centimeters"
defaults write NSGlobalDomain AppleMetricUnits -bool true


###############################################################################
# Screen                                                                      #
###############################################################################

# Require password immediately after sleep or screen saver begins
defaults write com.apple.screensaver askForPassword -int 1
defaults write com.apple.screensaver askForPasswordDelay -int 0

# Save screenshots to the screenshots folder
defaults write com.apple.screencapture location -string "${HOME}/Library/Mobile\ Documents/com~apple~CloudDocs/Screenshots"

# Save screenshots in PNG format
# (BMP, GIF, JPG, PDF, TIFF)
defaults write com.apple.screencapture type -string "png"

# Disable shadow in screenshots
defaults write com.apple.screencapture disable-shadow -bool true

# Enable subpixel font rendering on non-Apple LCDs
defaults write NSGlobalDomain AppleFontSmoothing -int 1


###############################################################################
# Finder                                                                      #
###############################################################################

# set Documents as the default location for new Finder windows
defaults write com.apple.finder NewWindowTarget -string "PfLo"
defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}/Documents"

# Show icons for external hard drives, servers, and removable media on the desktop
defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true
defaults write com.apple.finder ShowMountedServersOnDesktop -bool true
defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool true

# show hidden files by default
defaults write com.apple.finder AppleShowAllFiles -bool true

# show all filename extensions
defaults write NSGlobalDomain AppleShowAllExtensions -bool false

# hide status bar
defaults write com.apple.finder ShowStatusBar -bool false

# hide path bar
defaults write com.apple.finder ShowPathbar -bool false

# keep folders on top when sorting by name
defaults write com.apple.finder _FXSortFoldersFirst -bool true

# disable the warning when changing a file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

# enable spring loading for directories
defaults write NSGlobalDomain com.apple.springing.enabled -bool true

# avoid creating .DS_Store files on network or USB volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

# automatically open a new Finder window when a volume is mounted
defaults write com.apple.frameworks.diskimages auto-open-ro-root -bool true
defaults write com.apple.frameworks.diskimages auto-open-rw-root -bool true
defaults write com.apple.finder OpenWindowForNewRemovableDisk -bool true

# use list view in all Finder windows by default
# four-letter codes for the other view modes: `icnv`, `clmv`, `Flwv`
defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"

# Show the ~/Library folder
chflags nohidden ~/Library

# Show the /Volumes folder
sudo chflags nohidden /Volumes

# Expand the following File Info panes:
# “General”, “Open with”, and “Sharing & Permissions”
defaults write com.apple.finder FXInfoPanesExpanded -dict \
    General -bool true \
    OpenWith -bool true \
    Privileges -bool true


###############################################################################
# Dock, Dashboard, and Launchpad                                              #
###############################################################################

# increase amount of rows and columns in launchpad
defaults write com.apple.dock springboard-columns -int 8
defaults write com.apple.dock springboard-rows -int 5
defaults write com.apple.dock ResetLaunchPad -bool TRUE; killall Dock

# disable indicator lights for open applications in the Dock
defaults write com.apple.dock show-process-indicators -bool false

# don’t group windows by application in Mission Control
defaults write com.apple.dock expose-group-by-app -bool false

# don’t automatically rearrange Spaces based on most recent use
defaults write com.apple.dock mru-spaces -bool false

# remove the auto-hiding Dock delay
defaults write com.apple.dock autohide-delay -float 0

# reduce the animation when hiding/showing the Dock
defaults write com.apple.dock autohide-time-modifier -float 0.3

# automatically hide and show the Dock
defaults write com.apple.dock autohide -bool true

# don’t show recent applications in Dock
defaults write com.apple.dock show-recents -bool false


###############################################################################
# Safari & WebKit                                                             #
###############################################################################

# prevent Safari from opening "safe" files automatically after downloading
defaults write com.apple.Safari AutoOpenSafeDownloads -bool false

# Enable Safari’s debug menu
defaults write com.apple.Safari IncludeInternalDebugMenu -bool true

# Make Safari’s search banners default to Contains instead of Starts With
defaults write com.apple.Safari FindOnPageMatchesWordStartsOnly -bool false

# Enable the Develop menu and the Web Inspector in Safari
defaults write com.apple.Safari IncludeDevelopMenu -bool true
defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled -bool true

# Add a context menu item for showing the Web Inspector in web views
defaults write NSGlobalDomain WebKitDeveloperExtras -bool true

# enable continuous spellchecking
defaults write com.apple.Safari WebContinuousSpellCheckingEnabled -bool true
# disable auto-correct
defaults write com.apple.Safari WebAutomaticSpellingCorrectionEnabled -bool false

# disable AutoFill
defaults write com.apple.Safari AutoFillFromAddressBook -bool false
defaults write com.apple.Safari AutoFillPasswords -bool false
defaults write com.apple.Safari AutoFillCreditCardData -bool false
defaults write com.apple.Safari AutoFillMiscellaneousForms -bool false

# warn about fraudulent websites
defaults write com.apple.Safari WarnAboutFraudulentWebsites -bool true


###############################################################################
# Mail                                                                        #
###############################################################################

# display emails in threaded mode, sorted by date (oldest at the top)
defaults write com.apple.mail DraftsViewerAttributes -dict-add "DisplayInThreadedMode" -string "yes"
defaults write com.apple.mail DraftsViewerAttributes -dict-add "SortedDescending" -string "yes"
defaults write com.apple.mail DraftsViewerAttributes -dict-add "SortOrder" -string "received-date"

# disable inline attachments (just show the icons)
defaults write com.apple.mail DisableInlineAttachmentViewing -bool true


###############################################################################
# Terminal                                                                    #
###############################################################################

# change default shell to zsh
sudo chsh -s /bin/zsh

# only use UTF-8 in Terminal.app
defaults write com.apple.terminal StringEncodings -array 4

# enable Secure Keyboard Entry in Terminal.app
defaults write com.apple.terminal SecureKeyboardEntry -bool true

# disable line marks
defaults write com.apple.Terminal ShowLineMarks -int 0


###############################################################################
# Time Machine                                                                #
###############################################################################

# Prevent Time Machine from prompting to use new hard drives as backup volume
defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true


###############################################################################
# Activity Monitor                                                            #
###############################################################################

# Visualize CPU usage in the Activity Monitor Dock icon
defaults write com.apple.ActivityMonitor IconType -int 5


###############################################################################
# TextEdit                                                                    #
###############################################################################

# Use plain text mode for new TextEdit documents
defaults write com.apple.TextEdit RichText -int 0

# Open and save files as UTF-8 in TextEdit
defaults write com.apple.TextEdit PlainTextEncoding -int 4
defaults write com.apple.TextEdit PlainTextEncodingForWrite -int 4


###############################################################################
#  Disk Utility                                                               #
###############################################################################

# Enable the debug menu in Disk Utility
defaults write com.apple.DiskUtility DUDebugMenuEnabled -bool true
defaults write com.apple.DiskUtility advanced-image-options -bool true


###############################################################################
# Mac App Store                                                               #
###############################################################################

# Enable the automatic update check
defaults write com.apple.SoftwareUpdate AutomaticCheckEnabled -bool true

# Check for software updates daily, not just once per week
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1


###############################################################################
# Messages                                                                    #
###############################################################################

# Disable automatic emoji substitution (i.e. use plain text smileys)
defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add "automaticEmojiSubstitutionEnablediMessage" -bool false

# Disable smart quotes as it’s annoying for messages that contain code
defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add "automaticQuoteSubstitutionEnabled" -bool false


###############################################################################
# Other                                                                       #
###############################################################################

# disable "warning" when running python GUIs
defaults write org.python.python ApplePersistenceIgnoreState NO


echo "Done. Restart computer for all changes to take effect."
