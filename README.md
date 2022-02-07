#####Fortschritte

Im Script 'Daten_desrkiptiv' wurden die verschiedenen Daten eingelsesen, und aufbereitet, so dass diese mit dem join_Befehl zusammengefügt werden konnten. Die Daten sind so aufgeteilt, dass jede Reihe einem Land und einem Jahr zugeordnet werden können mit den Werten der entsrepchenden Variablen, wie dem HDI, GNI...

Eine ausführliche  Beschreibung der Variablen, die wichtig für die Konfliktforschung sind, können im Script 'Variablen Informationen' gefunden werden.

Data.frame mit allen Varaiblen die für die Ursachenforschung von Konflikten relevant sind -> Data_geo. Daten beinhalten Geographische Ortsangaben aus dem sf Packet für dei Visualisierung von Karten. Die Karten mit den Variablen können in dem Script Karten gefunden werden. 

Im Data_geo Data.frame sind alle Konflikte vom Jahr 1989 bis 2020 enthalten. Da in vielen Ländern mehrere Konflikte pro Jahr vorkommen, ist eine statistische Analyse der Daten schwierig. Im data.frame Stat_Data_geo sind pro Jahr die Todeszahlen summiert aller Konflikte summiert. Dementsrepchend hat jedes Land jede Jahreszahl nur einmal. 

Im Script 'Plots' sind alle Plots zufinden. Oben auf dem Scritpt ist ein Schnellzugang zu den Plots, die unten mit ggplot geschrieben sind. Die Plots sind in Kategorien aufgeteilt, die alle einen Einfluss auf Konflikte haben können. Die Variablen sind in 4 Plots analysiert. Zum einen eine Punktwolke, die mit einer Linearen Regression und einer "smooth" Funktion ausgestattet sind. Auf der X-Achse sind die Prediktor-Varibalen und der y-Achse die Anzahl der Toten in Konflikten pro Jahr und Land. Die Skala des y-Achsenabschnittes ist logarithmiert, da sich die Werte der Toten pro Jahr unterscheiden. Ein Konflikt ist erst als Konflikt definiert, wenn mindestens 25 personen sterben. 
-> Erste Analys Ergebnisse : GDP ist bei Ländern mit Konflikten höher als bei Ländern ohne... 

Im Script 'Karten' sind momentan karten zu sehen, die die Ausprägung der Prediktor-Variablen im Jahr 2019 für jedes Land einzeigen. Muss bei Bedarf weiter aktualisiert werden. 

Im Scipt 'Regression' sind statistische Analysen der Daten zu sehen. In Data_reg sind die Daten für die Regessionen Zusammengefasst. Anfangs wurde eine Hauptkomponentenanalyse unternommen. Die Variablen :"SP.POP.TOTL.FE.ZS","SP.POP.2529.MA.5Y","SG.GEN.PARL.ZS", "GDI", "HDI","EDI","Im_Ex","total_index_core","CC.EST","GE.EST","RQ.EST","RL.EST","VA.EST","GNI" wurden genutzt für die Analyse. Die ersten vier Kompontenten erklären ca. 80 Prozent der Varianz. ([1] 0.4915730578 0.1686082795 0.1018589942 0.0822062098). Im Plot Screenplot_prc ist die Verteilung der Komponenten zu erkennen. Hier ist schon zu erkennen, dass viele NAs in dem Datensatz die Analyse erstellten. Die PRC() Funktion kann mit NAs in den Reihen nicht umgehen. Es bleiben 95 Reihen übrig, die für die prc() verwendet werden wie in Data_prc_result$x zu sehen ist. 

Des Weitern wurde eine Multivaraite Lineare Regression durchgeführt. Auch wenn nicht alle Daten eine Linearität vorweisen.Das Lineare Model wurde mit den gleichern Prediktor-Variablen gespeist, wie in der Hauptkomponentenanalyse. Die Zielvariable ist Konflikt, die einge ausprägung von True und Fales hat. Im linearen Model sind Die Prediktoren Im_Ex, GDI, CC.EST und GE.EST extrem stark signifikant. Des Weiteren sind der Intercept, und die Variable SP.POP.TOTL.FE.ZS sind stark Signifikant. Der GNI und HDI zeigt eine schwache Signifikanz. Mit dem summary(Data_reg_lm) nachzugucken. Der folgende Outpunkt zeigt einen schwachen R°2 Wert. Es wird nur 19% der Varianz erklärt. 

Output:Residual standard error: 0.3279 on 1660 degrees of freedom
  (9365 observations deleted due to missingness)
Multiple R-squared:  0.1926,	Adjusted R-squared:  0.1858 
F-statistic: 28.28 on 14 and 1660 DF,  p-value: < 2.2e-16

Neben der Linearen Regression wird auch das generalized linear Model verwendet(glm). Dort kann die Family auf Binomial gestellt werden, da die Zielvariable eine binomiale Verteilung aufweist. Eine extrem starke Siginifikanz zeigen die Variablen Im_Ex, GDI, CC.EST und GE.EST. Der HDI hat im Vergleich zum lm an Signifikanz gewonnen. 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 1452.8  on 1674  degrees of freedom
Residual deviance: 1098.4  on 1660  degrees of freedom
  (9365 observations deleted due to missingness)
AIC: 1128.4

Number of Fisher Scoring iterations: 6

das Script 'tryICP' zeigt die Versuche Kausalitäten zu zeigen. X ist die matrix mit den Prediktorvariablen. Y1 ist ein Vektor mit den Konflikttoten pro Jahr. Y2 ist ein Vektor mit der binomialen Variabele Konflikt. ExpInd ist ein Vektor der verschiedene Interventions bzw. Environments beschreibt. In diesem Beispiel die Regionen der Welt. Ziel der Invarian Causal Projection (ICP) ist es kausale Zusammenhänge auf die Zielvariable auszurechenen. Der Environment Vektor(ExpInd) soll an den Variablen intervenieren. Es wird getestet, inwiefern  Y bei veränderten Einflüssen auf die Prediktorvariablen gleich bleibt. Gleichzeitig kann so die stärke des Zusammenhangs berechnet werden. Im Ergebniss kann gezeigt werden, ob eine Variable einen kauslane Zusammenhang auf Y hat oder nicht. Linare Regressionen und die resulrierende Signifikanz der Variablen sind anfäälig für falsche interpretationen und Effekten von Hintergrundvariablen. Die ICP zeigt Zusammenhängen, in dem die Kausalitäten zwischen den verschiedenen Variablen in verschiedenen Environments geprüft werden. Hintergrundvariablen können deswegen ausgeschlossen werden. Die Funktion heißt hiddenICP(). Das Environment kann beliebig gewählt werden, da Y nicht beeinflusst werden soll. Dementsprechend wurde ein zufälliger Vekotr erstellt, der Environments beschreibt (Exp_Ind_random)


Die ICP mit der Y2 ZielVariablen hat keine Signifikanz gezeigt, obwohl die Variablen, die in der lm extrem stark signifikant sind, die besten P-Werte vorweisen. summary(ICP_Y2).
Die ICP mit Y1 zeigt eine Siginifkanz bei der Variablen RL.EST. summary(ICP_Y1). 
Eine hiddenICP ist nicht möglich mit den Daten. Die output ist leer und. summary(hiddenICP2). Ist nicht relevant, ob Y1 oder Y2 benutzt werden. 

Liegt wahrscheinlich an den NAs im Dataframe. Versuch mit dem Dataframe ohne NAs zu arbeiten.
In der Tat verändert sich das Ergebniss vor allem bei der ICP mit der Y2 Variablen. Die Variablen GDI, SP.POP.TOTL.FE.ZS zeigen einen extrem signifikaten kausalen Zusammenhang mit Y2. GE.EST, VA.EST sind auch signifikant. Im_Ex, total_index_core und CC.EST weisen eine schwache Signifikanz vor. Bei der ICP ohne NAs wurde der ExpInd mit den WorldBank regionen verwendet.

Weiterhin wird jetzt die hiddenICP Funktion verwendet. 







Was ich noch brauche: Trendgrafik für Konflikte, Evtl. Heatmap von Konflikten. Expliziter auf Linearität prüfen. 
