package kr.ac.kaist.safe.analyzer.domain.react

import kr.ac.kaist.safe.analyzer.TypeConversionHelper
import kr.ac.kaist.safe.analyzer.domain.{AbsNum, AbsObj, AbsState, AbsStr, AbsValue, ConFin, ConSet, Loc, Num, fid2iv, locset2v, pv2v}

import scala.collection.immutable.Map

sealed abstract class CompDesc

// component description
case class ReactDesc(tag: AbsStr, props: AbsObj, children: List[CompDesc]) extends CompDesc {
  override def toString: String = "<" + tag.toString.replace("\"", "") + ">" +
    children.foldLeft("")((res, child) => res + child.toString) +
    "</" + tag.toString.replace("\"", "") + ">"
}

case class TextDesc(text: AbsStr) extends CompDesc {
  override def toString: String = text.toString.replace("\"", "")
}

// mounted component
case class MountedComp(comp: CompDesc, loc: Loc)

object ReactHelper {
  def extractCompDesc(elementValue: AbsValue, st: AbsState): CompDesc = {
    // string literal child
    if (elementValue.pvalue.strval != AbsStr.Bot) {
      TextDesc(elementValue.pvalue.strval)
    } else {
      val obj: AbsObj = st.heap.get(elementValue.locset)

      // compute tag string
      val (tagDesc, _) = obj.GetOwnProperty("type")
      val (tagVal, _) = tagDesc.value
      val tag: AbsStr = tagVal.pvalue.strval

      // compute props object
      val (propsDesc, _) = obj.GetOwnProperty("props")
      val (propsVal, _) = propsDesc.value
      val props: AbsObj = st.heap.get(propsVal.locset)

      // compute children object
      val (childrenDesc, undefChildren) = props.GetOwnProperty("children")
      val (childrenVal, _) = childrenDesc.value
      val childrenObj: AbsObj = st.heap.get(childrenVal.locset)

      // compute number of children
      val (numChildrenDesc, undefNumChildren) = childrenObj.GetOwnProperty("length")
      val (numChildrenVal, _) = numChildrenDesc.value
      val numChildren = TypeConversionHelper.ToNumber(numChildrenVal).gamma match {
        case ConFin(values) => {
          if (values.size >= 1) values.map(n => n.num).min.toInt
          else 0
        }
        case _ => 0
      }

      // recursively extract children from children object
      var children: List[CompDesc] = Nil
      if (numChildren > 0) {
        children = (0 to (numChildren - 1)).map(i => {
          val (childDesc, _) = childrenObj.GetOwnProperty(i.toString)
          val (childVal, _) = childDesc.value
          ReactHelper.extractCompDesc(childVal, st)
        }).toList
      }

      ReactDesc(tag, props, children)
    }
  }
}

object ReactState {
  // location generation
  // ideally, this would be integrated into the abstract state
  var nextLocIndex = -1
  def getNextLoc(): Loc = {
    nextLocIndex += 1
    Loc("React" + nextLocIndex)
  }

  var heap: Map[Loc, AbsObj] = Map[Loc, AbsObj]()
  var stateMap = Map[Loc, AbsObj]()
  var shapeMap = Map[Loc, (MountedComp, List[Loc])]()

  def render(comp: CompDesc): List[CompDesc] = {
    comp match {
      case ReactDesc(_, _, children) => children
      case TextDesc(text) => Nil
    }
  }

  def mount(comp: CompDesc): Loc = {
    val nextLoc = getNextLoc()
    val mountedComp = MountedComp(comp, nextLoc)
    stateMap += (nextLoc -> AbsObj.Empty)

    comp match {
      case ReactDesc(_, props, children) => {
        heap += (nextLoc -> props)
        mounted(mountedComp, mountSeq(children))
      }
      case _ => mounted(mountedComp, Nil)
    }
  }

  def mounted(mountedComp: MountedComp, locs: List[Loc]): Loc = {
    shapeMap += (mountedComp.loc -> (mountedComp, locs))
    mountedComp.loc
  }

  def mountSeq(comps: List[CompDesc]): List[Loc] = {
    comps match {
      case Nil => Nil
      case comp :: rest => mount(comp) :: mountSeq(rest)
    }
  }

  def unmount(loc: Loc): Unit = {
    shapeMap.get(loc) match {
      case None => Nil
      case Some((_, childLocs)) =>
        unmountSeq(childLocs)
        unmounted(loc)
    }
  }

  def unmountSeq(locs: List[Loc]): Unit = {
    locs match {
      case Nil => Nil
      case loc :: rest => {
        unmount(loc)
        unmountSeq(rest)
      }
    }
  }

  def unmounted(loc: Loc): Unit = {
    //  remove `loc` from listener map
  }

  override def toString: String = {
    "=======Heap=======\n\n" + heap.mkString("\n") + "\n" +
    "=======Shape map=======\n\n" + shapeMap.mkString("\n\n") + "\n\n"
  }
}
