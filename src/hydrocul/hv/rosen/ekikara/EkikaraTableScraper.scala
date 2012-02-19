package hydrocul.hv.rosen.ekikara;

import hydrocul.hv.http.HtmlPage;
import hydrocul.hv.http.Page;
import hydrocul.hv.http.WebBrowser;
import hydrocul.hv.rosen.TrainTime;
import hydrocul.hv.rosen.TrainTimePair;

object EkikaraTableScraper {

  case class StartEndIndex (
    startStation: String,
    endStation: String,
    startIndex: Int,
    endIndex: Int
  );

  def scrape(url: String, startEndList: Seq[StartEndIndex]):
    Seq[IndexedSeq[(TrainTimePair, Option[String])]] = {

    scrape(url, startEndList, { url: String =>
      WebBrowser.doGet(url);
    });

  }

  def scrape(url: String, startEndList: Seq[StartEndIndex],
    doGet: String => Page): Seq[IndexedSeq[(TrainTimePair, Option[String])]] = {

    scrape(url, startEndList, List.fill[IndexedSeq[(TrainTimePair, Option[String])]](
      startEndList.size)(Vector.empty[(TrainTimePair, Option[String])]), doGet);

  }

  private def scrape(url: String, startEndList: Seq[StartEndIndex],
    prevResult: Seq[IndexedSeq[(TrainTimePair, Option[String])]],
    doGet: String => Page): Seq[IndexedSeq[(TrainTimePair, Option[String])]] = {

    val page = doGet(url);

    scrape(page, startEndList, prevResult, doGet);

  }

  private def scrape(page: Page, startEndList: Seq[StartEndIndex],
    prevResult: Seq[IndexedSeq[(TrainTimePair, Option[String])]],
    doGet: String => Page): Seq[IndexedSeq[(TrainTimePair, Option[String])]] = {

    val htmlPage = page.asInstanceOf[HtmlPage];

    // HTMLの構造が変わったときに変わった箇所を判明しやすいように
    // CSSセレクタですべてを一度に書くのではなく、
    // 順番に要素をたどっていく
    val el1 = htmlPage.select("#container02");
    val result = {

      val el2 = el1.select("table:eq(9)");
      val el3 = el2.select("> tbody table:eq(0)");
      val el4 = el3.select("> tbody table:eq(0)");
      val trList = el4.select("> tbody > tr");
      val tdList1: IndexedSeq[Option[String]] = {
        val tdList = trList(4).select("td").drop(1);
        tdList.map { td =>
          try {
            val href = td.select("a").attr("href");
            hydrocul.hu.UrlUtil.createUrl(page.url, href);
          } catch { case e: IndexOutOfBoundsException =>
            None;
          }
        }
      }
      val tdList2: IndexedSeq[IndexedSeq[Option[TrainTime]]] = {
        val trList2 = trList.drop(6).dropRight(1);
          // trList2 は、
          // http://ekikara.jp/newdata/line/1310021/up1_1.htm
          // などで複数のtrが存在する可能性がある
        trList2.map { tr =>
          val tdList = tr.select("td").drop(2);
          tdList.map { td =>
            td.select("span").map { s =>
              try {
                Some(TrainTime(s.text.trim));
              } catch { case e: IllegalArgumentException =>
                None;
              }
            }
          }
        }.transpose.map(_.flatten);
      }
      val list: Seq[IndexedSeq[(TrainTimePair, Option[String])]] = startEndList.map { startEnd =>
        (tdList1 zip tdList2).map { case (detailUrlOpt, td) =>
          (td(startEnd.startIndex), td(startEnd.endIndex), detailUrlOpt)
        }.flatMap { t =>
          if(!t._1.isDefined || !t._2.isDefined){
            Nil
          } else {
            (TrainTimePair(t._1.get, t._2.get), t._3) :: Nil
          }
        }
      }
      prevResult.zip(list).map(r => r._1 ++ r._2);
    }

    {
      val el2 = el1.select("table:eq(8)");
      val a = el2.select("a:contains(次頁)");
      if(a.isEmpty){
        result;
      } else {
        val fname = a.attr("href");
        val url = page.url;
        val p = url.lastIndexOf('/');
        val nextUrl = url.substring(0, p) + "/" + fname;
        scrape(nextUrl, startEndList, result, doGet);
      }
    }

  }

  private[rosen] def test(all: Boolean): Seq[(String, Function0[Seq[Option[String]]])] = {
    if(!all){
      return Nil;
    }
    import hydrocul.hv.TestLib._;
    ("rosen.ekikara", { () =>
      val result = scrape("http://ekikara.jp/newdata/line/1310021/down1_1.htm",
        List(
          StartEndIndex("赤坂見附", "新宿三丁目", 13, 17),
          StartEndIndex("赤坂見附", "新宿", 13, 18)
        ));
      val result1 = result(0).take(2).map(_._1);
      val expected1 = Vector(TrainTimePair("05:26", "05:34"), TrainTimePair("05:33", "05:41"));
      val result2 = result(1).take(2).map(_._1);
      val expected2 = Vector(TrainTimePair("05:26", "05:35"), TrainTimePair("05:33", "05:42"));
      val result3 = result(1)(9);
      val expected3 = (TrainTimePair("06:31", "06:40"),
        Some("http://ekikara.jp/newdata/detail/1310021/142287.htm"));
      List(
        assertEquals(expected1, result1),
        assertEquals(expected2, result2),
        assertEquals(expected3, result3)
      );
    } ) :: Nil;
  }

}
