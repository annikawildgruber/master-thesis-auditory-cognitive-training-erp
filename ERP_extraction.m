%% BrainVision Peak Export einlesen 
clear; clc; close all;


baseDir = '/Users/annika/Library/CloudStorage/OneDrive-Persönlich/Documents/Studium/Master Schweiz/Thesis/Data/ERPs';
fileNames = {'Peaks_N100.txt', 'Peaks_P200.txt', 'Peaks_P50.txt'};

for i = 1:length(fileNames)
    fileName = fileNames{i};
    dataPath = fullfile(baseDir, fileName);
    
    % Prüfen, ob die Datei existiert
    if ~exist(dataPath, 'file')
        fprintf('Übersprungen (Nicht gefunden): %s\n', fileName);
        continue;
    end
    
    fprintf('\n>>> Verarbeite Datei: %s\n', fileName);
    
    % Peak-Namen extrahieren
    [~, nameWithoutExt, ~] = fileparts(fileName); % Ergibt 'Peaks_N100'
    peakName = erase(nameWithoutExt, 'Peaks_');    % Ergibt 'N100'
    
    opts = detectImportOptions(dataPath, 'FileType', 'text', 'NumHeaderLines', 1);
    opts = setvartype(opts, 'double');
    opts.VariableTypes{1} = 'string';     
    tbl = readtable(dataPath, opts);
    tbl.Properties.VariableNames = strrep(tbl.Properties.VariableNames, '-', '_');
    
    subject = tbl{:, 1}; 
    
    % Latenz-Spalte
    allCols = tbl.Properties.VariableNames;
    latColIdx = find(contains(allCols, 'L-Peak_Detection', 'IgnoreCase', true) | ...
                     (contains(allCols, 'L', 'IgnoreCase', true) & contains(allCols, 'Peak', 'IgnoreCase', true)));
                 
    if isempty(latColIdx)
        error('Konnte die Latenz-Spalte in %s nicht finden!', fileName);
    end
    
    latency_ms = tbl{:, latColIdx(1)}; 
    
    % Amplitude erstmal nur Cz
    Cz_amp_uV  = tbl{:, width(tbl)}; 
    amp = Cz_amp_uV; 
    Peak = repmat({peakName}, height(tbl), 1);
    
    peakTable = table(subject, latency_ms, amp, Peak, ...
        'VariableNames', {'subject', 'latency_ms', 'amp', 'Peak'});
    
    % Dezimaltrennzeichen für Excel-kompatiblen CSV-Export auf Komma setzen
    peakTable.amp = strrep(string(peakTable.amp), '.', ',');
    peakTable.latency_ms = strrep(string(peakTable.latency_ms), '.', ',');
    
    outFile = fullfile(baseDir, sprintf('%s_clean.csv', peakName));
    writetable(peakTable, outFile, 'Delimiter', ';');
    
    fprintf('Export erfolgreich: %s\n', outFile);
end

% Automatisiertes speichern im gleichen Ordner
fprintf('\n Alle verfügbaren Peak-Dateien wurden automatisch verarbeitet!\n');