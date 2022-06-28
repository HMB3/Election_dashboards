CREATE VIEW [Views].[BallotTrackCurrentStatus]

AS


WITH

max_date AS

(
	SELECT	[CONSKEY]
			,[LineNo]
			,MAX(StatusDate) AS StatusDate
	FROM [ElectionData].[Staging].[BTDB_NSW_ContainerStatus]
	GROUP BY [CONSKEY], [LineNo]

),

current_status AS (

SELECT	CONSTATa.[CONSKEY]
		,CONSTATa.StatusDate
		,CONSTATa.StatusStage
		,CONSTATa.StatusNotes
		,CONSTATa.LocationName
		,CONSTATa.[LineNo]

		 FROM [ElectionData].[Staging].[BTDB_NSW_ContainerStatus] CONSTATa
		 INNER JOIN max_date
		 ON CONSTATa.ConsKey = max_date.ConsKey
			AND CONSTATa.[LineNo] = max_date.[LineNo]
			AND CONSTATa.StatusDate = max_date.StatusDate
)



SELECT	TOP (100000) current_status.LocationName
		,CONNOTE.LGACode
		,CONNOTE.ContestName
		,current_status.StatusDate
		,current_status.StatusStage
		,current_status.StatusNotes
		,CONT.Barcode
		,CONT.Deleted

FROM current_status
	  LEFT JOIN [ElectionData].[Staging].[BTDB_NSW_Connote] CONNOTE
		ON current_status.[CONSKEY] = CONNOTE.[CONSKEY]
	  LEFT JOIN [ElectionData].[Staging].[BTDB_NSW_Container] CONT
		ON current_status.[CONSKEY] = CONT.[CONSKEY]
			AND current_status.[LineNo] = CONT.[LineNo]

ORDER BY current_status.LocationName
		,CONNOTE.LGACode
		,CONNOTE.ContestName
		,CONT.Barcode


