suite = {
  "mxversion" : "5.2.2",
  "name" : "graal-erl",
  # "url" : "http://openjdk.java.net/projects/graal",

  "imports" : {
    "suites" : [
        {
           "name" : "truffle",
           "version" : "bc1e026ef5b1ab8a1f68a3e78437e48f2d91fd91",
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
      "javaCompliance" : "1.8",
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
      "javaCompliance" : "1.8",
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
