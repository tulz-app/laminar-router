enablePlugins(ScalaJSPlugin)

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.7",
  "com.raquo" %%% "laminar" % "0.7",
  "com.lihaoyi" %%% "utest" % "0.6.7" % Test
)

scalaJSUseMainModuleInitializer := true

emitSourceMaps := false

testFrameworks += new TestFramework("utest.runner.Framework")