name := "FotM"

version := "2.0"

lazy val armory = project
lazy val watcher = project.dependsOn(armory)
