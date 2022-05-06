#####Fortschritte

Im Script 'Daten_desrkiptiv' wurden die verschiedenen Daten eingelsesen, und aufbereitet, so dass diese mit dem join_Befehl zusammengef�gt werden konnten. Die Daten sind so aufgeteilt, dass jede Reihe einem Land und einem Jahr zugeordnet werden k�nnen mit den Werten der entsrepchenden Variablen, wie dem HDI, GNI...

Eine ausf�hrliche  Beschreibung der Variablen, die wichtig f�r die Konfliktforschung sind, k�nnen im Script 'Variablen Informationen' gefunden werden.

Data.frame mit allen Varaiblen die f�r die Ursachenforschung von Konflikten relevant sind -> Data_geo. Daten beinhalten Geographische Ortsangaben aus dem sf Packet f�r die Visualisierung von Karten. Die Karten mit den Variablen k�nnen in dem Script Karten gefunden werden. 

Im Data_geo Data.frame sind alle Konflikte vom Jahr 1989 bis 2020 enthalten. Da in vielen L�ndern mehrere Konflikte pro Jahr vorkommen, ist eine statistische Analyse der Daten schwierig. Im data.frame Stat_Data_geo sind pro Jahr die Todeszahlen aller Konflikte summiert. Dementsprechend hat jedes Land jede Jahreszahl nur einmal. 

Im Script 'Plots' sind alle Plots zufinden. Oben auf dem Scritpt ist ein Schnellzugang zu den Plots, die unten mit ggplot geschrieben sind. Die Plots sind in Kategorien aufgeteilt, die alle einen Einfluss auf Konflikte haben k�nnen. Die Variablen werden in jeweils 4 Plots analysiert. Zum einen eine Punktwolke, die mit einer Linearen Regression und einer "smooth" Funktion ausgestattet sind. Auf der X-Achse sind die Prediktor-Varibalen und der y-Achse die Anzahl der Toten in Konflikten pro Jahr und Land. Die Skala des y-Achsenabschnittes ist logarithmiert, da sich die Werte der Toten pro Jahr unterscheiden. Ein Konflikt ist erst als Konflikt definiert, wenn mindestens 25 personen sterben. 
-> Erste Analys Ergebnisse : GDP ist bei L�ndern mit Konflikten h�her als bei L�ndern ohne... 

Im Script 'Karten' sind momentan Karten zu sehen, die die Auspr�gung der Prediktor-Variablen im Jahr 2019 f�r jedes Land einzeigen. Muss bei Bedarf weiter aktualisiert werden. 

Im Scipt 'Regression' sind statistische Analysen der Daten zu sehen. In Data_reg sind die Daten f�r die Regessionen zusammengefasst. Anfangs wurde eine Hauptkomponentenanalyse unternommen. Die Variablen :"SP.POP.TOTL.FE.ZS","SP.POP.2529.MA.5Y","SG.GEN.PARL.ZS", "GDI", "HDI","EDI","Im_Ex","total_index_core","CC.EST","GE.EST","RQ.EST","RL.EST","VA.EST","GNI" wurden genutzt f�r die Analyse. Die ersten vier Kompontenten erkl�ren ca. 80 Prozent der Varianz. ([1] 0.4915730578 0.1686082795 0.1018589942 0.0822062098). Im Plot Screenplot_prc ist die Verteilung der Komponenten zu erkennen. Hier ist schon zu erkennen, dass viele NAs in dem Datensatz die Analyse erschweren. Die PRC() Funktion kann mit NAs in den Reihen nicht umgehen. Es bleiben 95 Reihen �brig, die f�r die prc() verwendet werden wie in Data_prc_result$x zu sehen ist. 

Des Weitern wurde eine Multivaraite Lineare Regression durchgef�hrt. Auch wenn nicht alle Daten eine Linearit�t vorweisen.Das Lineare Model wurde mit den gleichern Prediktor-Variablen gespeist, wie in der Hauptkomponentenanalyse. Die Zielvariable ist Konflikt, die einge Auspr�gung von True und Fales hat. Im linearen Model sind Die Prediktoren Im_Ex, GDI, CC.EST und GE.EST extrem stark signifikant. Des Weiteren sind der Intercept, und die Variable SP.POP.TOTL.FE.ZS sind stark signifikant. Der GNI und HDI zeigt eine schwache Signifikanz. Mit dem summary(Data_reg_lm) nachzugucken. Der folgende Outpunkt zeigt einen schwachen R�2 Wert. Es wird nur 19% der Varianz erkl�rt. 

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

das Script 'tryICP' zeigt die Versuche Kausalit�ten zu zeigen. X ist die Matrix mit den Prediktorvariablen. Y1 ist ein Vektor mit den Konflikttoten pro Jahr. Y2 ist ein Vektor mit der binomialen Variabele Konflikt. ExpInd ist ein Vektor der verschiedene Interventions bzw. Environments beschreibt. In diesem Beispiel die Regionen der Welt. Ziel der Invarian Causal Projection (ICP) ist es kausale Zusammenh�nge auf die Zielvariable auszurechenen. Der Environment Vektor(ExpInd) soll an den Variablen intervenieren. Es wird getestet, inwiefern  Y bei ver�nderten Einfl�ssen auf die Prediktorvariablen gleich bleibt. Gleichzeitig kann so die st�rke des Zusammenhangs berechnet werden. Im Ergebniss kann gezeigt werden, ob eine Variable einen kausalen Zusammenhang auf Y hat oder nicht. Linare Regressionen und die resultierende Signifikanz der Variablen sind anf��lig f�r falsche interpretationen und Effekten von Hintergrundvariablen. Die ICP zeigt Zusammenh�ngen, in dem die Kausalit�ten zwischen den verschiedenen Variablen in verschiedenen Environments gepr�ft werden. Hintergrundvariablen k�nnen deswegen ausgeschlossen werden. Die Funktion hei�t hiddenICP(). Das Environment kann beliebig gew�hlt werden, da Y nicht beeinflusst werden soll. Dementsprechend wurde ein zuf�lliger Vekotr erstellt, der Environments beschreibt (Exp_Ind_random)


Die ICP mit der Y2 ZielVariablen hat keine Signifikanz gezeigt, obwohl die Variablen, die in der lm extrem stark signifikant sind, die besten P-Werte vorweisen. summary(ICP_Y2).
Die ICP mit Y1 zeigt eine Siginifkanz bei der Variablen RL.EST. summary(ICP_Y1). 
Eine hiddenICP ist nicht m�glich mit den Daten. Die output ist leer und. summary(hiddenICP2). Ist nicht relevant, ob Y1 oder Y2 benutzt werden. 

Liegt wahrscheinlich an den NAs im Dataframe. Versuch mit dem Dataframe ohne NAs zu arbeiten.
In der Tat ver�ndert sich das Ergebniss vor allem bei der ICP mit der Y2 Variablen. Die Variablen GDI, SP.POP.TOTL.FE.ZS zeigen einen extrem signifikaten kausalen Zusammenhang mit Y2. GE.EST, VA.EST sind auch signifikant. Im_Ex, total_index_core und CC.EST weisen eine schwache Signifikanz vor. Bei der ICP ohne NAs wurde der ExpInd mit den WorldBank regionen verwendet.

Weiterhin wird die hiddenICP Funktion verwendet. Die hiddenICP mit der binomialen Konflikt Variable Y2 zeigt, dass fast alle Variablen einen extrem starken kausalen Zusammenhang haben. Nur die Variablen RQ.EST, SG.GEN.PARL.ZS und GE.EST zeigen keine Signifikanz. print(hid_ICP_Y2_na) Die HiddenICP mit den Konflikt Todesopfern im Jahr zeigt ein anderes Bild. Hier haben die Variabeln GDI, Im_Ex, GNI, SG.GEN.PARL.ZS, SP.POP.2529.MA.5Y, RQ.EST, SP.POP.TOTL.FE.ZS einen kausalen Zusammenhang auf die Y Variable. 

Es ist wichtig welche Variablen genomen werden! GDI und GII haben einzelt einen kausalen Zusammenhang. Zusammen heben sie sich auf. Wird der HDI aus der Rechnung genommen, wir der GNI auch schwach signifikant. Der EDI aber nicht 





Was ich noch brauche: Trendgrafik f�r Konflikte, Evtl. Heatmap von Konflikten. Expliziter auf Linearit�t pr�fen und wenn n�tig eine Nichtlineare ICP durchf�hren.

Fragestellung: Welche Zusammenh�nge existieren zwischen sozio�konomischen Faktoren und dem Auftreten von Konflikten auf L�nderebene?

Hypothesen 1: Einzele Indizes und das Auftreten von Konflikten korrelieren miteinander? Es gibt einen positiven Zusammenhang zwischen einzelnen Indizes und dem Autreten von Konflikten
  - Plots f�r den Zusammenhang graphisch darstellen und einzele Korrelationen (greater oder less, pearson) -> geben keine angaben �ber die Richtung-> deswegen Plots 
  - Eine Multiple logit regression
Hypothese 2: Es gibt einen positiven kausalen Zusammenhang zwischen den Indizes und dem Auftreten von Konflikten 
  - hiddenICP mit den Indizes
  - Problem: Multikorrllinearit�t
Hypothese 3: Es gibt einen positiven kausalen Zusammenhang zwischen den Indikatoren der Indizes und dem Auftreten von Konflikten
 -  hiddenICP mit den Indikatoren 



patzial Korrelation berechen? (pcor) Rechnet eine Korrelation unabh�ngig von einer anderen Variablen aus (Gender Aspekte)





