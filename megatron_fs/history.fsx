
// History???

type history = { round:int; currentRoute:string;currentRouteStep:int; }

let writeHistory history =
    let jsonHistory = Newtonsoft.Json.JsonConvert.SerializeObject(history)
    System.IO.File.WriteAllText(historyFile, jsonHistory)

// Get history
    let history' =
        if argv.[0] = initialFile then 
            // Start of game
            if (System.IO.File.Exists(historyFile)) then System.IO.File.Delete(historyFile);
            let newHistory = 
                { 
                    round = 0;
                    currentRoute = "start";
                    currentRouteStep = 0;
                }
            writeHistory newHistory
            newHistory
        else
            let json = readFile historyFile
            Newtonsoft.Json.JsonConvert.DeserializeObject<history>(json)

// New History
    let history'' = 
        { 
            history' with 
                round = history'.round + 1;
                currentRouteStep = history'.currentRouteStep + 1;
        }
    writeHistory history''