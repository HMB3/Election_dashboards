GO



CREATE VIEW [Views].[BallotTrackHistoricalStatusByDistrictByType]
	AS


SELECT AREA.Area_Code,  BT.*, TC.TotalCons, TR.TotalRes, T2.[2.1.1] FROM [Staging].EMS_VW_EventAreaAuthorityLGWardIan AREA
LEFT JOIN (SELECT * FROM (
		SELECT CONSIGNMENT.LGACode, CONSIGNMENT.ContainerName, CONTAINERSTATUS.StatusStage, count(*) as TotalCount FROM BTDB.NSWECProd.dbo.NSW_Container CONTAINER
		INNER JOIN BTDB.NSWECProd.dbo.NSW_ContainerStatus CONTAINERSTATUS ON CONTAINERSTATUS.ConsKey = CONTAINER.ConsKey AND CONTAINERSTATUS.[LineNo] = CONTAINER.[LineNo]
		INNER JOIN BTDB.NSWECProd.dbo.NSW_Consignment CONSIGNMENT ON CONSIGNMENT.[ID] = CONTAINERSTATUS.[ConsKey] AND CONTAINERSTATUS.StatusNotes NOT LIKE '%Replaced By%'
		GROUP BY CONSIGNMENT.LGACode, CONSIGNMENT.ContainerName, CONTAINERSTATUS.StatusStage) A
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
											[6.11])) as MaxTotalCount) BT ON BT.LGACode = AREA.Area_Code COLLATE DATABASE_DEFAULT
									INNER JOIN
									(SELECT LGACode, ContainerName, SUM(ContainerQuantity) as TotalCons FROM BTDB.NSWECProd.dbo.NSW_Consignment
									WHERE VenueLongName IS NOT NULL  AND VenueName <> 'Sydney Town Hall'
									GROUP BY LGACode, ContainerName) TC ON TC.LGACode = BT.[LGACode] AND TC.ContainerName = BT.ContainerName
									LEFT JOIN
									(SELECT LGACode, ContainerName, SUM(ContainerQuantity) as TotalRes FROM BTDB.NSWECProd.dbo.NSW_Consignment
									WHERE VenueLongName IS NOT NULL  AND VenueName <> 'Sydney Town Hall' AND ContestName like '%RESERVE%'
									GROUP BY LGACode, ContainerName) TR ON TR.LGAcode = BT.[LGACode] AND TR.ContainerName = BT.ContainerName
									LEFT JOIN
									(SELECT Z.LGACode, count(*) as '2.1.1' from
									(
									SELECT DISTINCT C.LGACode, A.* FROM BTDB.NSWECProd.dbo.NSW_Consignment C
									INNER JOIN (
									SELECT  CSTATUS.ConsKey, CSTATUS.StatusNotes, CSTATUS.LocationName FROM BTDB.NSWECProd.dbo.NSW_ContainerStatus CSTATUS
									LEFT JOIN BTDB.NSWECProd.dbo.NSW_Consignment CONSIGNMENT ON CSTATUS.CONSKEY = CONSIGNMENT.ID
									WHERE CSTATUS.StatusStage = 2.1 AND CONSIGNMENT.ID IS NULL) A on A.LocationName like '%' + C.LGACode + '%'
									) Z GROUP BY LGACode) T2 ON T2.LGAcode = BT.[LGACode]