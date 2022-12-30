import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.String.Conversions
import Data.Yaml as Y ( encodeWith, defaultEncodeOptions, defaultFormatOptions, setWidth, setFormat)

import Lib1 (State(..), Square(..), emptyState)
import Lib2 (renderDocument, gameStart, hint)
import Lib3 (parseDocument)
import Types (Document(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  fromYamlTests,
  gameStartTests,
  hintTests,
  properties])

properties :: TestTree
properties = testGroup "Properties" [golden, dogfood]

friendlyEncode :: Document -> String
friendlyEncode doc = cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions) doc)

golden :: TestTree
golden = testGroup "Handles foreign rendering"
  [
    testProperty "parseDocument (Data.Yaml.encode doc) == doc" $
      \doc -> parseDocument (friendlyEncode doc) == Right doc
  ]

dogfood :: TestTree
dogfood = testGroup "Eating your own dogfood"
  [
    testProperty "parseDocument (renderDocument doc) == doc" $
      \doc -> parseDocument (renderDocument doc) == Right doc
  ]

fromYamlTests :: TestTree
fromYamlTests = testGroup "Document from yaml"
  [     testCase "null" $
          parseDocument "null\n" @?= Right DNull
      , testCase "int" $
          parseDocument "6\n" @?= Right (DInteger 6)
      , testCase "zero int" $
          parseDocument "0\n" @?= Right (DInteger 0)
      , testCase "non-empty string" $
          parseDocument "\"string\"\n" @?= Right (DString "string")
      , testCase "number as a string" $
          parseDocument "\"6\"\n" @?= Right (DString "6")
      , testCase "empty string" $
          parseDocument "''\n" @?= Right (DString "")
      , testCase "empty list" $
          parseDocument "---\n[]\n" @?= Right (DList [])
      , testCase "empty map" $
          parseDocument "---\n{}\n" @?= Right (DMap [])
      , testCase "list of ints" $
          parseDocument listOfInts @?= Right (DList [DInteger 5, DInteger 6])
      , testCase "list of nested labeled ints" $
          parseDocument listOfLabeledNestedInts @?= Right (DMap [("head",DInteger 5),("tail",DMap[("head",DInteger 6),("tail",DMap[("head",DInteger 7),("tail",DNull)])])])
      , testCase "list of coordinates" $
          parseDocument listOfCoordinates @?= Right (DList [DMap [("x",DInteger 5),("y",DInteger 6)],DMap [("x",DInteger 3),("y",DInteger 2)],DMap [("x",DInteger 4),("y",DInteger 5)]])
      , testCase "labeled list of ints" $
          parseDocument labeledListOfInts @?= Right (DMap [("ints",DList[DInteger 5, DInteger 6, DInteger 7])])
      , testCase "nested documents test 1" $
          parseDocument stringTestDocument1 @?= Right testDocument1
      , testCase "nested documents test 2" $
          parseDocument stringTestDocument2 @?= Right testDocument2
      , testCase "nested documents test 3" $
          parseDocument stringTestDocument3 @?= Right testDocument3
      , testCase "nested documents test 4" $
          parseDocument stringTestDocument4 @?= Right testDocument4
      , testCase "nested documents test 5" $
          parseDocument stringTestDocument5 @?= Right testDocument5
      , testCase "nested documents test 6" $
          parseDocument stringTestDocument6 @?= Right testDocument6
      , testCase "nested documents test 7" $
          parseDocument stringTestDocument7 @?= Right testDocument7
      , testCase "big document test" $
         parseDocument bigTestDocumentString2 @?= Right bigTestDocument2


  ]


testDocument1 :: Document
testDocument1 = DMap [("H",DString "V "),("QhBr",DMap [("nYgvZ",DMap [("IDltSV",DString "2591C  E z"),("QttDGbRRbS",DString "35 O  2  b2W  0 "),("hwKMOdE",DString "Q ")])]),("nudsQfpACn",DInteger (-64)),("H",DInteger 10)]

stringTestDocument1 :: String
stringTestDocument1 = unlines [
      "---"
    , "H: \"V \""
    , "QhBr: "
    , "  nYgvZ:"
    , "    IDltSV: \"2591C  E z\""
    , "    QttDGbRRbS: \"35 O  2  b2W  0 \""
    , "    hwKMOdE: \"Q \""
    , "nudsQfpACn: (-64)"
    , "H: 10"
    ]


testDocument2 :: Document
testDocument2 = DMap [("lxHW",DInteger 13),("P",DInteger (-24)),("P",DMap [])]

stringTestDocument2 :: String
stringTestDocument2 = unlines [
        "---"
      , "lxHW: 13"
      , "P: (-24)"
      , "P: {}"
    ]

testDocument3 :: Document
testDocument3 = DList [ DMap [("abc",DInteger 13),("b",DInteger (-24)),("c",DMap [])], DMap [("d", DInteger 4),("e", DList[DMap[("f", DMap [("g", DString "string")])]])], DMap [("h", DList []),("i",DMap []),("j",DInteger 0),("k",DString "")]]

stringTestDocument3 :: String
stringTestDocument3 = unlines [
        "---"
      , "- "
      , "  abc: 13"
      , "  b: (-24)"
      , "  c: {}"
      , "- "
      , "  d: 4"
      , "  e:"
      , "    - "
      , "      f: "
      , "        g: \"string\""
      , "- "
      , "  h: []"
      , "  i: {}"
      , "  j: 0"
      , "  k: ''"
    ]

testDocument4 :: Document
testDocument4 = DMap [("a",DList [DList [DNull]]),("b",DMap [("c",DNull)]),("d",DList [DMap [("e",DInteger 9)], DInteger 9])]

stringTestDocument4 :: String
stringTestDocument4 = unlines [
        "---"
      , "a: "
      , "  -"
      , "    - null"
      , "b: "
      , "  c: null"
      , "d: "
      , "  -"
      , "    e: 9"
      , "  - 9"
    ]


testDocument5 :: Document
testDocument5 = DMap [("sdsd",DList [DMap [("MGze",DInteger (-1))],DString "3"]),("DqeOwXC",DString "HLc0rnD337P99")]

stringTestDocument5 :: String
stringTestDocument5 = unlines [
    "---"
  , "sdsd: "
  , "  - "
  , "    MGze: -1"
  , "  - \"3\""
  , "DqeOwXC: \"HLc0rnD337P99\""
  ]



testDocument6 :: Document
testDocument6 = DList [DList [DList [DMap [("mxQ",DString " 9B"),("Q",DMap []),("Q",DInteger (-1))],DList [],DList [DInteger 2,DMap [("p",DString "2"),("K",DInteger 1),("g",DList [DList []])],DList [DList []]]],DString " b"],DList [DMap []]]


stringTestDocument6 :: String
stringTestDocument6 = unlines [
    "---"
  , "- "
  , "  - "
  , "    - "
  , "      mxQ: \" 9B\""
  , "      Q: {}"
  , "      Q: -1"
  , "    - []"
  , "    - "
  , "      - 2"
  , "      - "
  , "        p: \"2\""
  , "        K: 1"
  , "        g: "
  , "          - []"
  , "      - "
  , "        - []"
  , "  - \" b\""
  , "- "
  , "  - {}"


  ]

testDocument7 :: Document
testDocument7 = DMap [("HjNpY",DMap [("MOaI",DList []),("DobmO",DList [DList [DString "F 2",DMap [("F",DList []),("f",DMap [("fG",DList [DMap [("FHWL",DList [DInteger (-4)]),("w",DInteger 2),("EJ",DMap [("jJLDaN",DString ""),("zT",DString "Wpg kq7"),("F",DInteger 7),("F",DMap [("HskGuPu",DMap [("cUAzlOm",DString ""),("Plbfm",DString "rRV4N ")])])])],DMap [],DString "U c",DMap [("CKDf",DInteger 7),("Opze",DString "AN2"),("tg",DList [DList [DInteger 6,DList [DInteger 4]],DList [DString " A7gMZ4",DMap [("vcNnP",DString "KtF5u"),("DbmzYp",DList []),("cwoeNz",DInteger 1)],DString ""]])]])])]],DString "p  E",DList [DInteger (-7),DInteger (-7)],DMap []])])]

stringTestDocument7 :: String
stringTestDocument7 = unlines [
    "---"
  , "HjNpY: "
  , "  MOaI: []"
  , "  DobmO: "
  , "    - "
  , "      - \"F 2\""
  , "      - "
  , "        F: []"
  , "        f: "
  , "          fG: "
  , "            - "
  , "              FHWL: "
  , "                - -4"
  , "              w: 2"
  , "              EJ: " 
  , "                jJLDaN: ''"
  , "                zT: \"Wpg kq7\""
  , "                F: 7"
  , "                F: "
  , "                  HskGuPu: "
  , "                    cUAzlOm: ''"
  , "                    Plbfm: \"rRV4N \""
  , "            - {}"
  , "            - \"U c\""
  , "            - "
  , "              CKDf: 7"
  , "              Opze: \"AN2\""
  , "              tg: "
  , "                - "
  , "                  - 6"
  , "                  - "
  , "                    - 4"
  , "                - "
  , "                  - \" A7gMZ4\""
  , "                  - "
  , "                    vcNnP: \"KtF5u\""
  , "                    DbmzYp: []"
  , "                    cwoeNz: 1"
  , "                  - ''"
  , "    - \"p  E\""
  , "    - "
  , "      - -7"
  , "      - -7"
  , "    - {}"


  ]


bigTestDocumentString2 :: String
bigTestDocumentString2 = renderDocument bigTestDocument2


bigTestDocument2 :: Document
bigTestDocument2 = DMap [("rsypvoM",DMap [("GV",DMap [("FXPPtBXff",DList [DList [],DMap [("CEIV",DMap [("IgAmxNL",DString " D "),("mEA",DInteger (-40)),("TTU",DList [DInteger (-48)]),("PrRYSKw",DMap [])]),("OuCAl",DMap [("iFfdeBWQzj",DList []),("COqb",DInteger 4),("Y",DMap [("eioOh",DMap [("iY",DString "o8g8X "),("uhulAOAQTY",DInteger 36),("sITWSD",DString "9 748 O ")]),("xaidqtGLH",DList [DList [DList [DList [DMap [("wBlKMH",DString "4T16qRO58"),("o",DInteger (-48)),("UKyYoZK",DMap [("Xd",DString "av y")])]],DString "c dYJ F4jo9iZ"]],DInteger 29,DMap [("fWOUF",DMap [("OCoXMIxO",DList [DMap [("eQ",DMap [("SWcdunqB",DInteger (-49)),("Cj",DList [DMap [("ZwLtSAvfYT",DMap [("V",DList [DMap [("MGze",DInteger (-1))],DString "3"]),("DqeOwXC",DString "HLc0rnD337P99"),("fugWFc",DMap [("KhMkZyJY",DList [DMap [],DList [DMap [],DMap []]])]),("oTbSxdzRQ",DString "I")]),("lMlfBVZ",DInteger (-37)),("oh",DList [])],DString "  53q 5 14 U",DMap [("sQ",DList [DInteger (-13)]),("rSE",DList []),("oPWgKJFer",DInteger 24)]]),("eGMpFx",DString "28F 58 s"),("juZIxyiSc",DInteger (-9))]),("gMY",DList [DInteger 42,DInteger (-46),DMap [("hZcJh",DString "2i"),("MTtpScvRf",DString "Pf9I CI2 5J"),("daJaEndL",DMap [("N",DMap []),("almI",DString "s4P bE5")])]]),("vMUBAMso",DList [])],DString "",DString "8896 0x ",DInteger (-27)])]),("Vaoj",DList [DInteger (-6),DString "Pm4a81sO  eaq",DMap [("RBXijLwk",DList [DMap [("yPKiDcKy",DList [])]]),("PzKMew",DMap [("anGaQwQi",DList [DMap [("xQIPlPOQrd",DInteger (-44)),("pqDiHbzzja",DString ""),("yLsrmrDby",DString "lt Q"),("ZPuZMxtXE",DMap [("onvW",DMap [("jHXyawtr",DMap [("hefdRaZz",DInteger (-18)),("i",DInteger 13),("tKqOgH",DList [DMap [("e",DList [DList [DMap [("CQy",DMap [("Pef",DInteger 17),("fRFFNMgN",DInteger 43)]),("wyMxlgt",DMap [("aV",DInteger 25),("lL",DMap []),("Owv",DList [DString "0YQIa 7V ",DMap [("vOkQGrns",DString "q"),("CCA",DList [DString "L9Bu",DList [],DString "s BOUK cI8O  ",DList []]),("OFAJjZlu",DString ""),("CO",DInteger 36)]])]),("hopouriXog",DList [DString "jq2 K",DMap []])],DList [DList [DMap [("wY",DString "RC qQ9"),("VIzxPHvAB",DList [DString "  "]),("NgsSiK",DString "a "),("OqpsjJz",DMap [("Et",DString "c58hYf k7Zrpk "),("qAzYICH",DInteger (-7))])],DInteger 26],DInteger 12,DString " W8xM37Qgq u "],DMap [("i",DList [DInteger (-2),DInteger 32]),("FjplnGI",DInteger 43)],DMap [("bNpNAt",DString "iP1nOX2"),("NHjTuY",DMap [])]],DString "h87 Ad",DInteger 0]),("ZnpP",DMap [])],DMap [("ckC",DMap [("BTU",DList [DInteger (-32),DList [DList [],DMap [("sNlm",DMap [])],DMap [("c",DString "O4"),("mGWyV",DMap [])],DString "1vTpF2 Yzk"]]),("mMoN",DMap [("dmbb",DList [DInteger (-39),DList [DString " pCY N"],DInteger (-12),DList [DString " n48",DList [DInteger (-44),DMap [("eACEYSGjI",DInteger 29),("fwLzxtgFY",DList []),("EdGqUCcKm",DString "uc uolFP M")]],DList [DString "2Vfi6s"]]]),("NdGLOHaGeT",DInteger 6),("TUeWY",DMap [("X",DInteger 15),("X",DInteger (-33)),("yQVfQ",DString "8cx")])])]),("XlwgvaANNy",DString "yjC nO"),("qfZYZZ",DMap [("qwJUKrvIiC",DString "7UK g Yf")]),("m",DString "2h  l")],DString "b jS9k3e ZBQb7",DString "X  nVrh"]),("eizMnAFfby",DList [DMap [("e",DInteger 48),("lhKA",DList [DInteger (-38),DList [DInteger (-1),DString "O3dqpL r o 8yK"],DMap [("kSJ",DList [DInteger (-12)]),("FVSji",DMap [("sODhvQI",DMap [("AEuIpqkRn",DMap [("rNsfYnmdgr",DList [DString "HU7"]),("bnTniSbYiV",DList [DString "wgf0h D",DList [],DList [DMap [],DInteger 3]]),("ipCPNtoIT",DString "f1WlY i"),("mbmPqHjU",DList [DList [DInteger (-7),DList [DList [DInteger (-25)]],DList [DString "U3L ",DMap [("PEztW",DList []),("ZSZLats",DMap []),("dkjOwkBold",DInteger (-32))],DInteger (-33),DMap [("C",DList [DMap [("toV",DString "52r0g a "),("hzI",DList [DString "l z6m8i407",DString "Ou9 q  XC Pk  u",DInteger (-46),DList [DInteger (-46),DString "6VIaN6Q D0lm",DMap [],DList [DInteger (-23),DList [DMap [],DInteger 29,DInteger (-23),DList []],DList []]]]),("cqSKUNLD",DMap [("gPto",DString "5pA9 SG6 w31h"),("BHAfe",DInteger (-18)),("eQeTpBFpi",DMap [("JPmNdDVEm",DString "tp OMG T"),("dbkhbv",DString "9 Z 78 3 ml"),("t",DList [DMap [],DString " aHB w Z"])])]),("sLdrdgs",DMap [("qgtFFoq",DString "X  C4R1  N"),("k",DMap [("SumdOpW",DInteger (-3)),("LozNUkt",DMap [("SusbJNvR",DList [DString "4ot70L  JQ",DInteger (-31),DList [DList [DString "QM cp",DList [],DInteger 37,DInteger 29],DString " M2j uV 5",DInteger 3,DMap [("zDIsMQIB",DInteger 10),("Qgpt",DList []),("ARFmGzq",DInteger 14),("pdpHeShZ",DMap [("IZ",DInteger (-40))])]]]),("nJDebh",DInteger 36),("dfZPv",DMap [("enSFvfL",DString "J6E6h1ieP1K   "),("sllMW",DMap [("JatHv",DInteger (-8)),("XzQl",DInteger (-26))]),("pT",DInteger 5),("QZ",DInteger 14)])]),("HJtKfckHd",DList [DString "Q aYu",DInteger 15,DMap [("cnMIbnKOB",DInteger (-29)),("i",DMap [("gGTMMN",DString "sxN Y  Q1j0337q")]),("gsyJAdVm",DMap [("uQwBD",DInteger (-22)),("LhPQz",DList [DList [DMap []],DInteger 35]),("pOmUdBgVHn",DList [DMap [("BXVJcA",DInteger (-43)),("RFtwNfLQil",DInteger 8),("ZOBcREOb",DMap [("XkQ",DInteger (-17)),("G",DMap [("pPvAxh",DList [DInteger (-44),DMap []])]),("blufYS",DList []),("N",DMap [("zAdCrBcRd",DInteger 1),("k",DInteger (-33)),("gcKWAnzr",DString "gii  9aNh1a f"),("kiBJn",DList [DList [DMap [("hu",DList [DInteger (-20),DList [DList [DString " jZr r Q5 h ",DString "  XpO  313r 5e"],DMap [("tRbyfXv",DMap [("q",DMap [("JxQ",DString " aEQKW 1lYP0 3I"),("mR",DList []),("GtvIaUjTG",DList [DString "uEn0a  rH  N K",DList [DString "E8Og zx6k5NRw",DString "  r",DMap [("QrQ",DInteger 19)],DInteger 36]]),("NoyYoc",DList [DInteger 42,DInteger (-46),DString "zTX   "])]),("CSecHsqn",DList [DMap [("qZbHGPn",DMap [("ztQbxEiHzF",DInteger 42),("vxYjgc",DInteger (-10)),("kBrqXIVQrs",DMap [("CFUyLlF",DList [DMap [("RqZSmRGaf",DMap [("ikPShDVVGt",DString " eA9 Cl "),("ZAaoSfybOi",DList [DInteger 23,DList [DString "078j l030 m9"],DMap [],DList [DList [DInteger (-21),DInteger 31,DString "88t8Y  z   Hp R"],DString "bKp90O Mp",DMap [("jEClznwj",DList [])],DList [DInteger 1]]])]),("IGSuUUoP",DInteger (-44)),("aLuIp",DList [DInteger 13,DString "3uhxc84 f 3",DString "D gD242aH"])],DList [DList [DList [DList [DString "j0i  M B X0u",DList [DInteger 24,DString "4 ",DInteger (-15),DString "Dh G  t ZQ"]]],DInteger 4,DString "r"],DString "VJ5",DInteger 38,DList [DString "0 lho"]],DInteger (-47)]),("HOWaeJ",DMap [("JdLu",DMap [("vzVeMUtv",DInteger 44),("oD",DInteger (-13)),("fbU",DList [DList [DMap [("udLjWEQf",DList [DInteger 49,DMap [],DString " Xf "]),("npQO",DString "3ol6Z6d m   M m")],DInteger 40,DMap [("mR",DList [DList [DList [DInteger 39],DString "075 "],DMap [("QAd",DMap [("UEsEPnylTn",DString " 2  2q1676l"),("dqnqu",DString "  ")]),("FqNBRe",DString "c pl"),("ErIMGHrfm",DMap [("MP",DList [DList [DInteger (-17)],DInteger (-48),DMap [("jDCdrIEgCY",DString "E")],DList [DMap [("qSXJsZjkk",DList [])],DList []]])]),("VCrnVuZtV",DList [])],DMap []]),("npTN",DMap [("qPwIYpwkz",DString "Z5cZi 6 c"),("QdzQvDdkd",DList [DMap [("RTKcgS",DInteger 42),("BlG",DInteger 18),("lgtngi",DInteger (-2))],DString " iAkHfjHxUM  "])])]],DInteger 10])]),("obKFgYy",DList [DList [DInteger 2],DInteger 13,DList [DList [DList [DMap [("JqagalcGOK",DMap [])],DList [DInteger 28],DInteger (-31),DString "qvk4 2 0 E7Uv"],DString "Gz r A ",DString " 7F"],DString "4J ",DInteger 48,DInteger (-7)],DInteger 29]),("OufGA",DMap [("QInPQkxECH",DInteger (-43))])])]),("vnQqkBGgFm",DMap [])]),("d",DString "I1 gyfo8 sT t"),("T",DString "rg i")],DInteger (-48),DList [DInteger 41],DInteger 4])])]],DInteger 30,DMap [("CvZUqibKE",DMap [("aVrU",DString "7V J1C"),("IdNfLPPI",DList [DMap [("SBaGPFT",DInteger 35),("XSlRfuCx",DList [DMap [("hcfuLTVd",DMap []),("tCXIA",DInteger (-40)),("filkNEo",DList []),("PgZWCqFEMf",DList [DString "W ",DMap [("bPENp",DString "r YLVtPH"),("JkqqwUWu",DInteger 33),("FKLrS",DString "EGi1")]])],DList [DString "ysU5 dt8IZ D 0 "]]),("R",DString " 8CPk9AMuKh89bi"),("sHz",DMap [("Duu",DList [DString "Z22 lrshdl",DInteger 14,DInteger (-4)]),("bcgSlEG",DInteger 28),("JGtKKMUM",DString "  NA 2  "),("w",DInteger 27)])],DInteger (-2)])])]]),("iewLa",DList [DString "umLgD  U dw 7 7",DInteger (-22),DString " b19 31faCmK1N",DString "8c7X 2B"])],DMap [("rxaWg",DString "de5 SGhzclCm7"),("LdtJoCzzP",DInteger 2),("OjIOnT",DString "LIw"),("NSSIwdlsU",DMap [])],DInteger 11,DList [DString "",DMap [("LeFoliOGOJ",DMap []),("KNss",DMap [("YXmRAK",DMap [("CtW",DList []),("ql",DString " U    ")]),("Uf",DList [DMap [("y",DInteger 24),("PDctZLVm",DList [DInteger 22]),("efRI",DMap [("mOkmwtznR",DInteger (-37)),("tNqUJrtPz",DInteger 1),("KHsodA",DList [DInteger (-7)])])],DInteger (-21)]),("L",DMap []),("u",DString " 2d8DUi5")]),("whju",DMap [("oL",DList [DInteger 49,DString "VrM 3pkA7xz3p 6Y"]),("uo",DString "4j  k6J"),("OqQfW",DInteger (-12)),("grQhdVcA",DList [DMap [("tw",DInteger (-23)),("VhqWXEcTAs",DList [DList [DMap [("xIQauY",DList [DList [DList [DList [],DMap [("LgKFhsXMC",DMap []),("iHzfuPx",DString "4  ")]]],DMap [("HpBJy",DMap [("xqiLUxXu",DInteger 25)]),("fVsavHRmei",DString "8")]])],DList [DInteger (-16),DInteger (-6),DMap [("GA",DInteger (-41)),("dXdphL",DString "  k Y54"),("FeEw",DMap [("jAz",DList [DList [DMap [("yb",DString ""),("vPM",DInteger 40),("hojftZtx",DInteger 26)],DMap [("QollGMPZ",DInteger 44),("iu",DMap [("VUW",DList []),("m",DMap [("VHDI",DList [DInteger 40])]),("iR",DInteger 42)]),("HPhHMf",DString "z M i1M  46B W1A"),("nPrKatJRT",DList [DList [DInteger 30,DMap []],DMap [("dLWwNUhabG",DInteger 8),("HbvqlAMGbB",DInteger 23),("aEqwZz",DInteger (-44))],DMap [("PMAniAcp",DInteger 15),("jUiEiDpB",DList [DString "a  Z 6 6V24h",DString " R43  1Kh6rA k ",DList [DList [],DString "8 tO4Z07",DString "X 8A45L Yd "],DMap [("t",DString "7o nd6 c 50"),("FcyNJUqcQz",DInteger 24)]]),("zDBJBQ",DString "L   J4"),("iwdKx",DString "7myv9")],DString "KZ a"])],DInteger 28],DList [DInteger 29],DString "D1jH",DInteger 18]),("oY",DString " aP")])],DInteger 44],DList [DInteger (-24)],DString "MWc3"]]),("mu",DList [DString "   b96LwT72 "]),("xcXUAtcwXr",DMap [])],DInteger 28,DList [DList [DInteger (-3)],DString "s6C1B k ",DMap [("axwE",DString "7 O Jb I AD"),("yRAMqnM",DString ""),("nowrDgGfZC",DInteger (-47))],DString ""],DMap [("ssdCR",DString "62"),("LrvB",DString "    Ipk  u  ")]])])],DList [DMap [("xyXY",DInteger 0)],DString "Z wml 7 0Yw1V"]]],DMap [("b",DList [DList [DMap [("JkiY",DString ""),("TI",DString "  3  D"),("LXLYtrmww",DMap [("swCKnbpWWe",DString "j5R Z J  T 7r B"),("IqHEAOCt",DInteger (-10))]),("YhWFziIvDa",DMap [("CjdjbFj",DMap [("kbLF",DList [DString "x01pn"]),("iHNmL",DMap [("QVre",DString "d55f9Tmw"),("cGuifPn",DString "S8w A  64  w p"),("lzHKbfzOF",DInteger 6)]),("QQeZx",DInteger 35),("cuLOLSLt",DInteger 43)]),("VUsckI",DList [DList [DString "9 3Dc9Y  08rz",DList []]])])],DList [DList [],DList [DInteger 7],DInteger 50,DString "9 08T 3 3IBo"],DMap [("MCzMA",DInteger (-25)),("OjWzr",DString "xj BL  vk")],DMap [("GBzEsfMCfT",DInteger 40),("V",DInteger (-34)),("rHETys",DInteger 16),("qCApUvgj",DString "")]]]),("X",DString "R  nA6lJHF")]])])]),("D",DMap [("mhWyff",DList [DInteger 6,DInteger 39,DList [DString "R   Pu7z7o2NM5",DMap [],DString "E2   6 fO"]])])],DList [DList [],DMap [("MlZxOzYx",DString "d 7nIG  9f h3 "),("vIa",DInteger 18),("t",DString ""),("gsJvkOrKoD",DMap [("uft",DList [DList [DInteger 8,DInteger 3],DString "jgu90lX 465  6"]),("gOVHawIZoh",DString " yvx3H tM  "),("SaEJ",DString "DTq2ZeN H  RbFoh")])],DInteger 16,DInteger 24],DList [DString "X7GS2oD   "]])]),("cZZkk",DString "1   8W  8x2Hz ud")],DInteger (-50)]),("FoRfGHSl",DList [])]),("KgnQh",DMap [])])],DList [DInteger (-44),DMap [("KurZBVhuzN",DList [DList [DMap [("euQooEWnpi",DList []),("vODc",DList [DString "N p0S",DInteger (-5),DInteger (-44)]),("SygCVrsbA",DInteger 19)],DInteger (-1),DString "T9Oo    9V cSX",DInteger 41],DList [DInteger (-14)]])]]]),("aTjdlVI",DMap [("sZAhnrq",DString ""),("VdUuJvMu",DList [DMap [],DMap [("BtcWmnq",DString " ZD2iI 51")]])]),("qnYDecBaMz",DMap [])]],DInteger 7]])]),("NPzf",DInteger (-6)),("g",DString "NN0 H"),("AUUbNwzp",DInteger (-31))]),("CsZX",DMap []),("TPuollLz",DString "b  U "),("OCrrrV",DMap [("QLRR",DMap [("EEsTY",DString " xCTIU9")]),("o",DString "zdH k11aL3sp"),("yrvVOCDZ",DInteger 17),("AZ",DMap [("vxw",DList [DList [],DMap [("ZSVnZp",DList [DList [DMap [],DString " "],DList [DMap [("zXXawVP",DMap [("VozmyZRuBA",DInteger 39),("VdbZwEmWN",DString "R vK"),("oYoScoB",DString "2 t")]),("lLnv",DInteger 50),("T",DString "7  9P 2"),("bJPhHofRr",DMap [("RsM",DInteger 48),("vYGiCn",DString " m"),("kw",DString "4S33"),("GFdyh",DString "1 ir  x")])]],DInteger (-24),DInteger 6])]]),("zAAzMxxu",DMap [("WxM",DInteger (-28)),("VcS",DInteger 41)])])])]),("m",DMap [("OQywye",DString "1BWACnV  Uh"),("oYIboKTDu",DString "SO3 a8jh"),("Zatpetw",DInteger 34)])]])],DString " R C1q F6Y  q"])]),("qhk",DList [DInteger (-33),DList [DList [DList [],DMap [("nkDFjR",DString "e8z 1i6X"),("laIL",DString "7q  3n Q")],DList [DList [DMap [("kcb",DInteger (-6))]],DInteger (-29),DString "d  N3   52 "],DMap [("ewCEiznJ",DInteger 8)]],DList [DList [DString "  o3"]],DString " az6Go "],DString "20iD 91hU"]),("yNshxPzw",DString "0  l13 "),("CVyGaZeS",DList [DList [],DMap [("VjTQ",DMap [("PHOHlX",DList [])])],DString "XJF WbYP "])]),("TXTq",DList [DString "K FS8  vC3A2WFu6",DInteger 30]),("m",DInteger (-46))])]]),("WhFsZywH",DInteger 12),("WPQUYIi",DMap [("pu",DInteger 32),("p",DInteger 10)]),("g",DMap [("J",DMap []),("nKlRfgZ",DMap [("QhA",DMap [("j",DInteger 6)])]),("JUXtE",DList [])])]),("bMiWSJe",DInteger 37)]]),("tgML",DList [DList [DList [DMap [("TI",DMap [("RNurIluY",DString "nt9PDdMm9 ")])],DString "b3 g"],DInteger (-21),DList [DMap [("orY",DList [DMap [("uvgHqepPJz",DMap [("PkQbBrAx",DList [DString "X g",DString "J 8 ",DString "jl0Os3st  d",DMap [("kOmdK",DInteger 16),("uF",DString "1  yFw0  k6a")]])]),("tyuryqfBO",DString "u")]]),("NIeeoWGZnb",DString "eL  H"),("bmUCF",DString "v")],DList [DMap [("FdqYOykI",DList [DInteger 27]),("hCYCCqr",DString " ye6tW UEtet")],DInteger (-12),DMap [("voAdUrQat",DList [DInteger 38,DInteger (-47)]),("mBQTn",DList [DInteger 34,DString "uMc",DInteger (-15)])]],DString "2endto2svk"]],DString "o QC m a  2b54k1"])]])]),("I",DList [DInteger 24,DList []])]),("r",DString " h sD"),("mNMGpsis",DInteger 37)],DInteger (-49),DList [DList [DInteger 31,DString "20  8",DMap [("WQSWbf",DMap [("uOPRGFo",DInteger 50),("VULPuqCT",DString " SD0s8DtCf  z ")]),("t",DList [DInteger (-21)]),("McMq",DString "QSZ 9JAlub s")]],DMap [("OtHfOvbYoI",DInteger (-20)),("MFXOzNzjgf",DString "4uu LU0hHS8"),("YjFpgDItxe",DMap [("lk",DMap [("EI",DInteger (-7)),("FCTQGosHIL",DMap [("mRVtLwtu",DMap [("OEoYQDtBZw",DMap [("mTuAf",DString "lv  "),("foScbbI",DString " B"),("dJv",DList [DMap [("DcPDUOlLD",DList [DString " 37 15mKe6 O",DList [],DList [DList [DString "tK98",DString "Xwn "],DList []],DString "l4C0f3 vu"]),("mHf",DInteger 46),("hrillVZj",DMap [("jL",DString "M yl J3r5O a"),("V",DString "NmE6s 55X "),("erGoNHB",DString "K 2Mb70159L e "),("BKWLq",DInteger 37)]),("sEDc",DString "5  N  2YM 34")],DInteger (-31)]),("Ys",DMap [("kig",DString " ZWm UEW")])]),("QgHXeQU",DList [DInteger 29]),("LiIM",DMap [("NKXBsy",DString "5"),("suzWmj",DMap [])])]),("NPlaL",DString "h  v c 5O41 ")])])])],DInteger (-22),DString " "]]),("X",DList [DList [DInteger 16,DString "9zWnq7"]]),("WxXioB",DInteger (-46)),("yzKaojxz",DMap [("aEgNp",DString " HLJS5d A bJ ")])]),("kDZawTq",DString ""),("mAnSorJ",DInteger (-36))])]





toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "null" $
        renderDocument DNull @?= "null"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "5"
    , testCase "empty string" $
        renderDocument (DString "") @?= "''"
    , testCase "non-empty string" $
        renderDocument (DString "demo string") @?= "\"demo string\""
     , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
     , testCase "list of nested labeled ints" $
        renderDocument (DMap [("head",DInteger 5),("tail",DMap[("head",DInteger 6),("tail",DMap[("head",DInteger 7),("tail",DNull)])])]) @?= listOfLabeledNestedInts
     , testCase "list of coordinates" $
        renderDocument (DList [DMap [("x",DInteger 5),("y",DInteger 6)],DMap [("x",DInteger 3),("y",DInteger 2)],DMap [("x",DInteger 4),("y",DInteger 5)]]) @?= listOfCoordinates
     , testCase "labeled list of ints" $
        renderDocument (DMap [("ints",DList[DInteger 5, DInteger 6, DInteger 7])]) @?= labeledListOfInts
     , testCase "big document test" $ 
        renderDocument bigTestDocument2 @?= bigTestDocumentString2 
     , testCase "nested documents test 5" $ 
        renderDocument testDocument5 @?= stringTestDocument5 
     , testCase "nested documents test 6" $ 
        renderDocument testDocument6 @?= stringTestDocument6
     , testCase "nested documents test 7" $ 
        renderDocument testDocument7 @?= stringTestDocument7

      

  ]


listOfInts :: String
listOfInts = unlines [
      "---"
    , "- 5"
    , "- 6"
  ]

listOfLabeledNestedInts :: String
listOfLabeledNestedInts = unlines [
      "---"
    , "head: 5"
    , "tail: "
    , "  head: 6"
    , "  tail: "
    , "    head: 7"
    , "    tail: null"
  ]

listOfCoordinates :: String
listOfCoordinates = unlines [
      "---"
    , "- "
    , "  x: 5"
    , "  y: 6"
    , "- "
    , "  x: 3"
    , "  y: 2"
    , "- "
    , "  x: 4"
    , "  y: 5"
  ]
labeledListOfInts :: String
labeledListOfInts = unlines [
      "---"
    , "ints: "
    , "  - 5"
    , "  - 6"
    , "  - 7"
  ]

gameStartTests :: TestTree
gameStartTests = testGroup "Test start document"
  [    testCase "Game start document" $
          gameStart emptyState gameStartDocument @?= Right gameStartState
      , testCase "Unable to find DMap at the game start" $
          gameStart emptyState (DList []) @?= Left "Unable to find DMap."
      , testCase "Unable to find element with the key 'occupied_rows'" $
          gameStart emptyState gsdWithoutRowKey @?= Left "There's no such element with the key: occupied_rows"
      , testCase "Unable to find row' data" $
          gameStart emptyState gsdWithEmptyRowData @?= Left "Unable to find row' or col' data."
      , testCase "Unable to find 'head' key" $
          gameStart emptyState gsdWithoutHeadKey @?= Left "Key is invalid."
      , testCase "Out of range head DInteger Value" $
          gameStart emptyState gsdOutOfRangeHeadValue @?= Left "DInteger x expected for \"head\", 0 < x < 10"
      , testCase "Invalid head value" $
          gameStart emptyState gsdHeadInvalidValue @?= Left "DInteger expected for \"head\""
      , testCase "Unable to find 'tail' key" $
          gameStart emptyState gsdWithoutTailKey @?= Left "Key is invalid."
      , testCase "Invalid tail value" $
          gameStart emptyState gsdTailInvalidValue @?= Left "DMap or DNull is expected for \"tail\""
      , testCase "Hint number is invalid" $
          gameStart emptyState gsdWithInvalidHintValue @?= Left "\"number_of_hints\" expected as DInteger"
      , testCase "Hint number is negative value" $
          gameStart emptyState gsdWithInvalidHintNumber @?= Left "\"number_of_hints\" expected to be positive number"
      , testCase "Row data list length not equal 10" $
          gameStart emptyState gsdToLessRowData @?= Left "Expected 10 values for rows and for columns."
  ]

gameStartState :: State
gameStartState = State {
    rowData = [3,3,0,6,0,2,2,2,0,2],
    colData = [1,1,2,3,1,4,2,4,2,0],
    numHints = 10,
    hintCoords = [],
    board = replicate 100 Water,
    document = gameStartDocument
}

number_of_hints :: (String,Document)
number_of_hints = ("number_of_hints",DInteger 10)

occupied_cols :: (String,Document)
occupied_cols = ("occupied_cols",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 3),("tail",DMap [("head",DInteger 1),("tail",DMap [("head",DInteger 4),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 4),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DNull)])])])])])])])])])])

occupied_rows :: (String,Document)
occupied_rows = ("occupied_rows",DMap [("head",DInteger 3),("tail",DMap [("head",DInteger 3),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 6),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DNull)])])])])])])])])])])

game_setup_id :: (String,Document)
game_setup_id = ("game_setup_id",DString "d3993d51-041d-4c86-aa69-a6eb85c41e11")

gameStartDocument :: Document
gameStartDocument = DMap [
            number_of_hints,
            occupied_cols,
            occupied_rows,
            game_setup_id
            ]
gsdWithoutRowKey :: Document
gsdWithoutRowKey = DMap [
            number_of_hints,
            occupied_cols,
            game_setup_id
            ]
gsdWithEmptyRowData :: Document
gsdWithEmptyRowData = DMap [
            number_of_hints,
            occupied_cols,
            ("occupied_rows",DMap []),
            game_setup_id
            ]

gsdWithoutHeadKey :: Document
gsdWithoutHeadKey = DMap [
            number_of_hints,
            occupied_cols,
            ("occupied_rows",DMap [("h",DInteger 3),("tail",DMap [("h",DInteger 3),("tail",DMap [("h",DInteger 0),("tail",DMap [("h",DInteger 6),("tail",DMap [("h",DInteger 0),("tail",DMap [("h",DInteger 2),("tail",DMap [("h",DInteger 2),("tail",DMap [("h",DInteger 2),("tail",DMap [("h",DInteger 0),("tail",DMap [("h",DInteger 2),("tail",DNull)])])])])])])])])])]),
            game_setup_id
            ]

gsdOutOfRangeHeadValue :: Document
gsdOutOfRangeHeadValue = DMap [
            number_of_hints,
            occupied_cols,
            ("occupied_rows",DMap [("head",DInteger 20),("tail",DMap [("head",DInteger 3),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 6),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DNull)])])])])])])])])])]),
            game_setup_id
            ]

gsdHeadInvalidValue :: Document
gsdHeadInvalidValue = DMap [
            number_of_hints,
            occupied_cols,
            ("occupied_rows",DMap [("head",DString "string"),("tail",DMap [("head",DInteger 3),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 6),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DNull)])])])])])])])])])]),
            game_setup_id
            ]

gsdWithoutTailKey :: Document
gsdWithoutTailKey = DMap [
            number_of_hints,
            occupied_cols,
            ("occupied_rows",DMap [("head",DInteger 3),("t",DMap [("head",DInteger 3),("t",DMap [("head",DInteger 0),("t",DMap [("head",DInteger 6),("t",DMap [("head",DInteger 0),("t",DMap [("head",DInteger 2),("t",DMap [("head",DInteger 2),("t",DMap [("head",DInteger 2),("t",DMap [("head",DInteger 0),("t",DMap [("head",DInteger 2),("t",DNull)])])])])])])])])])]),
            game_setup_id
            ]

gsdTailInvalidValue :: Document
gsdTailInvalidValue = DMap [
            number_of_hints,
            occupied_cols,
            ("occupied_rows",DMap [("head",DInteger 3),("tail",DList [])]),
            game_setup_id
            ]


gsdWithInvalidHintValue :: Document
gsdWithInvalidHintValue = DMap [
            ("number_of_hints",DString "string"),
            occupied_cols,
            occupied_rows,
            game_setup_id
            ]

gsdWithInvalidHintNumber :: Document
gsdWithInvalidHintNumber = DMap [
            ("number_of_hints",DInteger (-1)),
            occupied_cols,
            occupied_rows,
            game_setup_id
            ]

gsdToLessRowData :: Document
gsdToLessRowData = DMap [
            number_of_hints,
            occupied_cols,
            ("occupied_rows",DMap [("head",DInteger 3),("tail",DMap [("head",DInteger 2),("tail",DMap [("head",DInteger 0),("tail",DMap [("head",DInteger 2),("tail",DNull)])])])]),
            game_setup_id
            ]

hintTests :: TestTree
hintTests = testGroup "Test hint document" [
    testCase "All hints to game start document" $
      hint gameStartState hint_coords @?= Right (hintState gameStartState)
  , testCase "Unable to find DMap" $
      hint gameStartState (DList[]) @?= Left "Unable to find DMap."
  , testCase "Unable to find coords by key" $
      hint gameStartState hint_coords_without_key @?= Left "There's no such element with the key: coords"
  , testCase "Unable to find DList for hint coords" $
      hint gameStartState hint_coords_without_dlist @?= Left "DList is expexted."
  , testCase "Unable to find DList values for hint coords" $
      hint gameStartState hint_coords_with_empty_dlist @?= Left "DList expected to be not empty."
  , testCase "Unable to find DMap for single coord" $
      hint gameStartState hint_coords_list_without_dmaps @?= Left "Hint coordinates document is invalid."
  , testCase "Couldn't find 'col' key" $
      hint gameStartState hint_coord_without_col_key @?= Left "Key is invalid."
  , testCase "Couldn't find 'row' key" $
      hint gameStartState hint_coord_without_row_key @?= Left "Key is invalid."
  , testCase "Given not DInteger for coord data" $
      hint gameStartState hint_coord_without_DInteger @?= Left "Hint coordinates document is invalid."
  , testCase "Given DInteger for coord data out of range" $
      hint gameStartState hint_coord_number_out_of_range @?= Left "Hint coordinate is out of bounds."
  ]


-- only for test case "All hints to game start document"
hintState :: State -> State
hintState s = s {
  board = [Water,Water,Water,Water,Ship,Ship,Ship,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Ship,Ship,Ship,Ship,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Ship,Water,Water,Water,Water,Water,Water,Water,Water,Water,Ship,Water,Water,Water,Water,Water,Water,Water,Water,Water,Ship,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water,Water],
  hintCoords = [(6,0),(5,0),(4,0),(7,7),(7,6),(7,5),(5,3),(6,3),(7,3),(8,3)]
}

hint_coords :: Document
hint_coords = DMap [("coords",DList [DMap [("col",DInteger 8),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 3)],DMap [("col",DInteger 6),("row",DInteger 3)],DMap [("col",DInteger 5),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 5)],DMap [("col",DInteger 7),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 7)],DMap [("col",DInteger 4),("row",DInteger 0)],DMap [("col",DInteger 5),("row",DInteger 0)],DMap [("col",DInteger 6),("row",DInteger 0)]])]

hint_coords_without_key :: Document
hint_coords_without_key = DMap [("c",DList [DMap [("col",DInteger 8),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 3)],DMap [("col",DInteger 6),("row",DInteger 3)],DMap [("col",DInteger 5),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 5)],DMap [("col",DInteger 7),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 7)],DMap [("col",DInteger 4),("row",DInteger 0)],DMap [("col",DInteger 5),("row",DInteger 0)],DMap [("col",DInteger 6),("row",DInteger 0)]])]

hint_coords_without_dlist :: Document
hint_coords_without_dlist = DMap [("coords", DMap [])]

hint_coords_with_empty_dlist :: Document
hint_coords_with_empty_dlist = DMap [("coords", DList [])]

hint_coords_list_without_dmaps :: Document
hint_coords_list_without_dmaps = DMap [("coords", DList [DString "demo"])]

hint_coord_without_col_key :: Document
hint_coord_without_col_key = DMap [("coords",DList [DMap [("c",DInteger 8),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 3)],DMap [("col",DInteger 6),("row",DInteger 3)],DMap [("col",DInteger 5),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 5)],DMap [("col",DInteger 7),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 7)],DMap [("col",DInteger 4),("row",DInteger 0)],DMap [("col",DInteger 5),("row",DInteger 0)],DMap [("col",DInteger 6),("row",DInteger 0)]])]

hint_coord_without_row_key :: Document
hint_coord_without_row_key = DMap [("coords",DList [DMap [("col",DInteger 8),("r",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 3)],DMap [("col",DInteger 6),("row",DInteger 3)],DMap [("col",DInteger 5),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 5)],DMap [("col",DInteger 7),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 7)],DMap [("col",DInteger 4),("row",DInteger 0)],DMap [("col",DInteger 5),("row",DInteger 0)],DMap [("col",DInteger 6),("row",DInteger 0)]])]

hint_coord_without_DInteger :: Document
hint_coord_without_DInteger = DMap [("coords",DList [DMap [("col",DString "demo"),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 3)],DMap [("col",DInteger 6),("row",DInteger 3)],DMap [("col",DInteger 5),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 5)],DMap [("col",DInteger 7),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 7)],DMap [("col",DInteger 4),("row",DInteger 0)],DMap [("col",DInteger 5),("row",DInteger 0)],DMap [("col",DInteger 6),("row",DInteger 0)]])]

hint_coord_number_out_of_range :: Document
hint_coord_number_out_of_range = DMap [("coords",DList [DMap [("col",DInteger 20),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 3)],DMap [("col",DInteger 6),("row",DInteger 3)],DMap [("col",DInteger 5),("row",DInteger 3)],DMap [("col",DInteger 7),("row",DInteger 5)],DMap [("col",DInteger 7),("row",DInteger 6)],DMap [("col",DInteger 7),("row",DInteger 7)],DMap [("col",DInteger 4),("row",DInteger 0)],DMap [("col",DInteger 5),("row",DInteger 0)],DMap [("col",DInteger 6),("row",DInteger 0)]])]
