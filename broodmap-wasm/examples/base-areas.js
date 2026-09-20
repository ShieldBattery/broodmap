import { hasBuildableTerrainNearMidpoint } from './boundary-buildability.js'

// Scheduling and presentation only. The reusable Rust job owns all topology decisions.
export async function analyzeBaseAreas(
  analysis,
  bases,
  minWideningPercent,
  { onProgress = () => {}, checkpoint = async () => {} } = {},
) {
  await checkpoint()
  const anchors = bases.map(({ id, routeAnchor }) => ({ id, routeAnchor }))
  const job = analysis.beginBaseTopology(JSON.stringify(anchors), minWideningPercent)
  try {
    let complete = false
    let completedOrigins = 0
    while (!complete) {
      await checkpoint()
      complete = job.advance()
      const progress = JSON.parse(job.progressJson())
      if (progress.completedOrigins !== completedOrigins) {
        completedOrigins = progress.completedOrigins
        onProgress({
          completedOrigins,
          totalOrigins: progress.totalOrigins,
          surveyCount: progress.surveyCount,
        })
      }
    }
    await checkpoint()
    const result = JSON.parse(job.metadataJson())
    const flags = analysis.cellFlags()
    result.boundaries = result.boundaries.map(boundary => ({
      ...boundary,
      buildableNearMidpoint: hasBuildableTerrainNearMidpoint(boundary, analysis.widthWalkTiles, flags),
    }))
    return { ...result, labels: job.labels().buffer }
  } finally {
    job.free()
  }
}
