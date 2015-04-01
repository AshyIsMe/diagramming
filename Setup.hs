import Distribution.MacOSX
import Distribution.Simple

main = defaultMainWithHooks $ simpleUserHooks {
         postBuild = appBundleBuildHook guiApps
       }

guiApps :: [MacApp]
guiApps = [MacApp "diagramming"
                  (Just "resources/WxHello.icns")
                  Nothing   -- Default Info.plist
                  []        -- No other resources
                  []        -- No other binaries
                  DoNotChase
          ]
