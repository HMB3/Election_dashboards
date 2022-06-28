--GO
--CREATE VIEW [Views].[BallotTrackHistoricalStatusByVenue]
	--AS
--	
SELECT AREA.Area_Code, BT.*, TC.TotalCons, ISNULL(TR.TotalRes, 0) as 'TotalRes', 0 as '2.1.1' FROM [Staging].EMS_VW_EventAreaAuthorityLGWardIan AREA
LEFT JOIN (SELECT * FROM (
		SELECT CONSIGNMENT.LGACode, CONSIGNMENT.VenueName, CONTAINERSTATUS.StatusStage, count(*) as TotalCount FROM DV_Staging_BallotTrack_2021.dbo.NSW_Container CONTAINER
		INNER JOIN DV_Staging_BallotTrack_2021.dbo.NSW_ContainerStatus CONTAINERSTATUS ON CONTAINERSTATUS.ConsKey = CONTAINER.ConsKey AND CONTAINERSTATUS.[LineNo] = CONTAINER.[LineNo]
		INNER JOIN DV_Staging_BallotTrack_2021.dbo.NSW_Consignment CONSIGNMENT ON CONSIGNMENT.[ID] = CONTAINERSTATUS.[ConsKey]
		GROUP BY CONSIGNMENT.LGACode, CONSIGNMENT.VenueName, CONTAINERSTATUS.StatusStage) A
		PIVOT (
					   MAX(TotalCount)
					   FOR StatusStage in ([1.00],
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
											[6.11])) as MaxTotalCount) BT ON BT.LGACode = AREA.Area_Code COLLATE DATABASE_DEFAULT
									INNER JOIN
									(SELECT LGACode, VenueName, SUM(ContainerQuantity) as TotalCons FROM DV_Staging_BallotTrack_2021.dbo.NSW_Consignment
									WHERE VenueLongName IS NOT NULL and ContainerName = 'Carton' AND VenueName <> 'Sydney Town Hall'
									GROUP BY LGACode, VenueName) TC ON TC.LGACode = BT.[LGACode] AND TC.VenueName = BT.VenueName
									LEFT JOIN
									(SELECT LGACode, VenueName, SUM(ContainerQuantity) as TotalRes FROM DV_Staging_BallotTrack_2021.dbo.NSW_Consignment
									WHERE VenueLongName IS NOT NULL and ContainerName = 'Carton' AND VenueName <> 'Sydney Town Hall' AND ContestName like '%RESERVE%'
									GROUP BY LGACode, VenueName) TR ON TR.LGAcode = BT.[LGACode] AND TR.VenueName = BT.VenueName