suite = {
  "mxversion" : "5.2.2",
  "name" : "graal-erl",
  # "url" : "http://openjdk.java.net/projects/graal",

  "imports" : {
    "suites" : [
        {
           "name" : "truffle",
           "version" : "371045b1312d",#"37fabf84537a53b7994b9b8867bd3f4439dfc775",
           "urls" : [
                {"url" : "http://lafo.ssw.uni-linz.ac.at/hg/truffle", "kind" : "hg"},
                {"url" : "http://lafo.ssw.uni-linz.ac.at/nexus/content/repositories/snapshots", "kind" : "binary"},
            ]
        },
    ],
  },

  "defaultLicense" : "GPLv2-CPE",
  "libraries" : {
  },

  "projects" : {

    "com.oracle.truffle.erl" : {
      "subDir" : "graal-erl",
      "sourceDirs" : ["src"],
      "dependencies" : [
        "truffle:TRUFFLE_API",
      ],
      "javaCompliance" : "1.7",
      "annotationProcessors" : ["truffle:TRUFFLE_DSL_PROCESSOR"],
      "workingSets" : "Truffle,Erlang",
      "license" : "UPL",
    },

    "com.oracle.truffle.erl.test" : {
      "subDir" : "graal-erl",
      "sourceDirs" : ["src"],
      "dependencies" : [
        "truffle:TRUFFLE_TCK",
        "com.oracle.truffle.erl",
      ],
      "checkstyle" : "com.oracle.truffle.erl",
      "javaCompliance" : "1.7",
      "workingSets" : "Truffle,Erlang,Test",
      "license" : "UPL",
    },
  },

  "licenses" : {
    "UPL" : {
      "name" : "Universal Permissive License, Version 1.0",
      "url" : "http://opensource.org/licenses/UPL",
    }
  },
}
