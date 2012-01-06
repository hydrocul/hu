package hydrocul;

package object hv {

  private[hv] def testPackage: () => Seq[Option[String]] = { () =>
    testCipherUtil.apply() ++
    testEncodingMania.apply();
  }

  private[hv] def testCipherUtil: () => Seq[Option[String]] = { () =>
    import TestLib._;
    import CipherUtil._;
    val a = encodeCipher("abc".getBytes, "あいうえお".getBytes("UTF-8"));
    val b = new String(decodeCipher("abc".getBytes, a), "UTF-8");
    List(
      assertEquals("07b91211306447be8244146ad5d07a03", encodeHex(a)),
      assertEquals("あいうえお", b)
    );
  }

  private[hv] def testEncodingMania: () => Seq[Option[String]] = { () =>
    import TestLib._;
    import EncodingMania._;
    List(
      assertEquals("97,98,99",
        thru("abc").mkString(",")),
      assertEquals("-29,-127,-126,-29,-127,-124,-29,-127,-122",
        encodeChar("あいう", "UTF-8").mkString(","))
    );
    // TODO テストケースが不十分
  }

}
