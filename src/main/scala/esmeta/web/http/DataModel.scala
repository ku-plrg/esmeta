package esmeta.web.http

object DataModel:
  type BpData = (Boolean, String, List[Int], Boolean)
  type AddBreakpointRequest = BpData
  type RunRequest = (String, List[BpData])
  type ResumeFromIterRequest = (String, List[BpData], Int)
