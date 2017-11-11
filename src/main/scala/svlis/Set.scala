package svlis

class Set(val contents: Int, val prim: Primitive) {
  def this(prim: Primitive) = this(prim.contents, prim)
  def this(a: Int) = this(a, new Primitive())

  // A single plane is a convex polygon - sv_c_flag detects this
  // TODO set_flags(sv_c_flag(p));

  def range() = {
    // TODO return Interval with logic from set.cxx 1114-1173
  }

  def prune(b: Box): Set = {
    val pruned = if (contents <= Contents.Nothing) { // includes Everything
      this
    } else if (contents == 1) {
      val m = prim.range(b).member()
      m match {
        case MemTest.Air => new Set(Contents.Nothing)
        case MemTest.Surface => this
        case MemTest.Solid => new Set(Contents.Everything)
        case _ => this // TODO svlis_error("sv_set::prune(sv_box)", "dud mem test", SV_CORRUPT)
      }
//      switch (m)
//      {
//        case SV_AIR:
//        pruned = sv_set(SV_NOTHING);
//        break;
//        case SV_SURFACE:
//        pruned = *this;
//        break;
//        case SV_SOLID:
//        pruned = sv_set(SV_EVERYTHING);
//        break;
//        default:
//          svlis_error("sv_set::prune(sv_box)", "dud mem test", SV_CORRUPT);
//      }
    } else {
      this
      // TODO logic from set.cxx 1216-1254
    }
    //if(reg_prune) pruned = pruned.regularize();
    //return(att_prune(pruned, *this, b));
    pruned
  }
}