name := "ScalaCategoryTheory"

version := "1.0"

scalaVersion := "2.11.11" //"2.12.0-RC1"

traceLevel := -1

logLevel := Level.Info

// disable printing timing information, but still print [success]
showTiming := false

//added here when suggested by sbt in command line
//ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

// disable printing a message indicating the success or failure of running a task
showSuccess := false

// append -deprecation to the options passed to the Scala compiler
scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:postfixOps", "-Ypartial-unification")

// disable updating dynamic revisions (including -SNAPSHOT versions)
offline := true


//For the Kind projector plugin
resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")


libraryDependencies ++= Seq(
     "org.apache.commons" % "commons-lang3" % "3.12.0", // was 3.6
     //Scala Reflections
     "org.scala-lang" % "scala-reflect" % "2.11.11",
     //ScalaCheck
     "org.scalacheck" %% "scalacheck" % "1.15.2" % Test, // was 1.13.5
     //Specs2
     "org.specs2" %% "specs2-core" % "4.10.6" % Test, // was 4.0.2
     "org.specs2" %% "specs2-scalacheck" % "4.10.6" % Test, // was 4.0.2
     //Discipline
     "org.typelevel" %% "discipline" % "0.11.1", //was 0.8
     //Spire
     "org.typelevel" %% "spire" % "0.16.2", // was 0.14.1
     "org.typelevel" %% "spire-laws" % "0.16.2", /// was 0.14.1
     //Algebra
     "org.typelevel" %% "algebra" % "2.0.0" % Test, // was 0.7.0
     "org.typelevel" %% "algebra-laws" % "2.0.0" % Test, // was 0.7.0
     //Scalaz
     "org.scalaz"      %% "scalaz-core"    % "7.3.5", // 7.3.0-M19
     //Cats
     //"org.typelevel"   %% "cats"           % "1.0.1",
     "org.typelevel" %% "cats-core" % "2.0.0",
     "org.typelevel"   %% "cats-macros"           % "2.0.0",
     "org.typelevel"   %% "cats-kernel"           % "2.0.0", //TODO was 1.0.1 - evictions
     "org.typelevel"   %% "cats-laws"           % "2.0.0",
     "org.typelevel"   %% "cats-free"           % "2.0.0",
     "org.typelevel"   %% "cats-testkit"           % "2.0.0",
     //Shapeless
     "com.chuusai"     %% "shapeless"      % "2.3.7", // was 2.3.3
     //Kind projector plugin
     "org.spire-math" %% "kind-projector" % "0.9.9" // was 0.9.4
)
