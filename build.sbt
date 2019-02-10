enablePlugins(ScalaJSPlugin)

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.6",
  "com.raquo" %%% "laminar" % "0.6",
  "com.lihaoyi" %%% "utest" % "0.6.6" % Test
)

scalaJSUseMainModuleInitializer := true

emitSourceMaps := false

testFrameworks += new TestFramework("utest.runner.Framework")