----GO
----CREATE VIEW [Views].[BallotTrackStatusByVenue]
	----AS 
SELECT * FROM (
SELECT CONSIGNMENT.LGACode, CONSIGNMENT.VenueName, CONTAINERSTATUS.StatusStage, count(*) as TotalCount FROM DV_Staging_BallotTrack_2021.dbo.NSW_Container CONTAINER
INNER JOIN DV_Staging_BallotTrack_2021.dbo.NSW_ContainerStatus CONTAINERSTATUS ON CONTAINERSTATUS.ConsKey = CONTAINER.ConsKey AND CONTAINERSTATUS.[LineNo] = CONTAINER.[LineNo]
INNER JOIN
(select cs.ConsKey, cs.[LineNo], Max(StatusTime) as StatusTime from DV_Staging_BallotTrack_2021.dbo.NSW_ContainerStatus cs
GROUP BY cs.ConsKey, cs.[LineNo]) CURRSTATUS on CURRSTATUS.ConsKey = CONTAINERSTATUS.ConsKey AND 
										        CURRSTATUS.[LineNo] = CONTAINERSTATUS.[LineNo] AND
												CURRSTATUS.[StatusTime] = CONTAINERSTATUS.[StatusTime]
INNER JOIN DV_Staging_BallotTrack_2021.dbo.NSW_Consignment CONSIGNMENT ON CONSIGNMENT.[ID] = CONTAINERSTATUS.[ConsKey]
GROUP BY CONSIGNMENT.LGACode, CONSIGNMENT.VenueName, CONTAINERSTATUS.StatusStage) A
PIVOT (
			   MAX(TotalCount)
			   FOR [StatusStage] in ([1.00],
				[2.00],
				[2.10],
				[3.00],
				[4.00],
				[5.00],
				[5.08],
				[5.09],
				[5.10],
				[5.11],
				[5.12],
				[5.13],
				[5.14],
				[5.15],
				[5.16],
				[5.17],
				[5.18],
				[6.00],
				[6.10],
				[6.11])) as MaxTotalCount

