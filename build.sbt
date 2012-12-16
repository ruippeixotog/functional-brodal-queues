seq(Revolver.settings: _*)

javaOptions in Revolver.reStart ++= Seq("-Xmx4g", "-Xms4g", "-Xmn400M")

libraryDependencies += "org.specs2" %% "specs2" % "1.12.1" % "test"
