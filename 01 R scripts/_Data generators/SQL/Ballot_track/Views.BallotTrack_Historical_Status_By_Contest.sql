GO
CREATE VIEW [Views].[BallotTrackHistoricalStatusByContest]
	AS

SELECT MaxTotalCount.*, tC.TotalCons, ISNULL(TR.TotalRes, 0) as TotalRes, MaxTotalCount.[2.10] as '2.1.1' FROM (
SELECT CONSIGNMENT.LGACode, ISNULL(CONSIGNMENT.ContestName, 'Unknown') as ContestName,  CONTAINERSTATUS.StatusStage, count(*) as TotalCount FROM BTDB.NSWECProd.dbo.NSW_Container CONTAINER
INNER JOIN BTDB.NSWECProd.dbo.NSW_ContainerStatus CONTAINERSTATUS ON CONTAINERSTATUS.ConsKey = CONTAINER.ConsKey AND CONTAINERSTATUS.[LineNo] = CONTAINER.[LineNo]
INNER JOIN BTDB.NSWECProd.dbo.NSW_Consignment CONSIGNMENT ON CONSIGNMENT.[ID] = CONTAINERSTATUS.[ConsKey]
GROUP BY CONSIGNMENT.LGACode, ISNULL(CONSIGNMENT.ContestName, 'Unknown'), CONTAINERSTATUS.StatusStage) A
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
INNER JOIN
(SELECT LGACode, ISNULL(ContestName, 'Unknown') 'ContestName', SUM(ContainerQuantity) as TotalCons FROM BTDB.NSWECProd.dbo.NSW_Consignment
WHERE VenueLongName IS NOT NULL and ContainerName = 'Carton' AND VenueName <> 'Sydney Town Hall'
GROUP BY LGACode, ISNULL(ContestName, 'Unknown')) TC ON TC.LGACode = MaxTotalCount.[LGACode] AND TC.ContestName = MaxTotalCount.ContestName
LEFT JOIN
(SELECT LGACode, ISNULL(ContestName, 'Unknown') 'ContestName', SUM(ContainerQuantity) as TotalRes FROM BTDB.NSWECProd.dbo.NSW_Consignment
WHERE VenueLongName IS NOT NULL and ContainerName = 'Carton' AND VenueName <> 'Sydney Town Hall' AND ContestName like '%RESERVE%'
GROUP BY LGACode, ISNULL(ContestName, 'Unknown')) TR ON TR.LGAcode = MaxTotalCount.[LGACode] AND TR.ContestName = MaxTotalCount.ContestName



