package kr.ac.kaist.safe.analyzer.domain.react

import kr.ac.kaist.safe.analyzer.domain.{AbsObj, AbsValue, Loc}
import scala.collection.immutable.Map

case class CompDesc(renderedElt: AbsValue, props: AbsObj, children: List[CompDesc])

case class MountedComp(comp: CompDesc, loc: Loc)

class ReactState(getNextLoc: () => Loc) {
  var heap: Map[Loc, AbsValue] = Map[Loc, AbsValue]()
  var stateMap = Map[Loc, AbsObj]()
  var shapeMap = Map[Loc, (MountedComp, List[Loc])]()

  def render(comp: CompDesc): CompDesc = comp

  def mount(comp: CompDesc): Loc = {
    val nextLoc = getNextLoc()
    val mountedComp = MountedComp(comp, nextLoc)
    stateMap += (nextLoc -> AbsObj.Empty)
    mounted(mountedComp, mountSeq(comp.children))
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
}
