seq(Revolver.settings: _*)

javaOptions in Revolver.reStart += "-Xmx4g"

libraryDependencies += "org.specs2" %% "specs2" % "1.12.1" % "test"
