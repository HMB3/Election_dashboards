---GO
---CREATE VIEW [Views].[BallotTrackHistoricalStatusByVenueByContest]
	---AS
---
SELECT A.LGACode, A.VenueName, A.ContestName, 
			B.[1.00], B.[2.00], b.[2.10], B.[3.00], b.[4.00], 
			b.[5.00], B.[5.08], B.[5.09], B.[5.10], B.[5.11], B.[5.12], B.[5.13], B.[5.14], B.[5.15], B.[5.16], B.[5.17], B.[5.18],
			B.[6.00], B.[6.10], b.[6.11], 
			A.TotalCons, 0 as 'TotalRes', B.[2.10] as '2.1.1' FROM
(
SELECT LGACode, VenueName, ContestName, sum(ContainerQuantity) as 'TotalCons' FROM DV_Staging_BallotTrack_2021.dbo.NSW_Consignment
group by LGACode,VenueName, ContestName
) A
LEFT JOIN
(
SELECT CONSIGNMENT.LGACode, ISNULL(CONSIGNMENT.ContestName, 'Unknown') as ContestName, CONSIGNMENT.VenueName, CONTAINERSTATUS.StatusStage, count(*) as TotalCount FROM DV_Staging_BallotTrack_2021.dbo.NSW_Container CONTAINER
LEFT JOIN DV_Staging_BallotTrack_2021.dbo.NSW_ContainerStatus CONTAINERSTATUS ON CONTAINERSTATUS.ConsKey = CONTAINER.ConsKey AND CONTAINERSTATUS.[LineNo] = CONTAINER.[LineNo]
INNER JOIN DV_Staging_BallotTrack_2021.dbo.NSW_Consignment CONSIGNMENT ON CONSIGNMENT.[ID] = CONTAINERSTATUS.[ConsKey]
GROUP BY CONSIGNMENT.LGACode, ISNULL(CONSIGNMENT.ContestName, 'Unknown'), CONSIGNMENT.VenueName, CONTAINERSTATUS.StatusStage) A
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
									[6.11])
					) B ON A.LGACode = B.LGACode AND A.VenueName = B.VenueName AND A.ContestName = B.ContestName
					WHERE A.LGAcode IS NOT NULL

