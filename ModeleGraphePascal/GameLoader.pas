{*******************************************************************************
  GameLoader.pas
  Chargement des données du jeu depuis les fichiers CSV

  Fonctions principales :
  - LoadGameData : Charge l'image, les nœuds et les voies d'un projet
  - ParseNodesCSV : Parse le fichier dataGraph.csv
  - ParseEdgesCSV : Parse le fichier voie.csv
*******************************************************************************}
{$mode objfpc}{$H+}

unit GameLoader;

interface

uses
  SysUtils,
  Classes,
  raylib,
  GameConfig,
  GameTypes;

// Charge toutes les données d'un projet (image + CSV)
// Retourne True si le chargement a réussi, False sinon
function LoadGameData(const ProjectName: String;
                      out Texture: TTexture2D;
                      out Nodes: TNodeArray;
                      out Edges: TEdgeArray): Boolean;

implementation

// === FONCTIONS PRIVEES DE PARSING ===

// Parse une ligne CSV et retourne un tableau de strings
function SplitCSV(const Line: String; const Delimiter: Char): TStringList;
var
  i: Integer;
  CurrentField: String;
begin
  Result := TStringList.Create;
  CurrentField := '';

  for i := 1 to Length(Line) do
  begin
    if Line[i] = Delimiter then
    begin
      Result.Add(CurrentField);
      CurrentField := '';
    end
    else
      CurrentField := CurrentField + Line[i];
  end;

  // Ajouter le dernier champ
  if CurrentField <> '' then
    Result.Add(CurrentField);
end;

// Parse le fichier dataGraph.csv et remplit le tableau de nœuds
function ParseNodesCSV(const FilePath: String; out Nodes: TNodeArray): Boolean;
var
  FileHandle: TextFile;
  Line: String;
  Fields: TStringList;
  Node: TNode;
  LineNum: Integer;
begin
  Result := False;
  SetLength(Nodes, 0);

  if not SysUtils.FileExists(FilePath) then
  begin
    // WriteLn('ERREUR: Fichier nodes introuvable: ', FilePath);
    Exit;
  end;

  try
    AssignFile(FileHandle, FilePath);
    Reset(FileHandle);
    LineNum := 0;

    while not Eof(FileHandle) do
    begin
      ReadLn(FileHandle, Line);
      Inc(LineNum);

      // Ignorer la ligne d'en-tête
      if LineNum = 1 then
        Continue;

      // Ignorer les lignes vides
      if Trim(Line) = '' then
        Continue;

      Fields := SplitCSV(Line, ';');
      try
        // Vérifier qu'on a au moins 12 champs
        if Fields.Count >= 12 then
        begin
          try
            // Parser les données
            Node.ID := StrToInt(Fields[0]);
            Node.X := StrToInt(Fields[1]);
            Node.Y := StrToInt(Fields[2]);
            Node.Shape := StrToInt(Fields[3]);
            Node.Dim1 := StrToInt(Fields[4]);
            Node.Dim2 := StrToInt(Fields[5]);
            Node.Name := Fields[6];  // Peut être vide
            Node.VP := StrToInt(Fields[7]);
            Node.Height := StrToInt(Fields[8]);
            Node.Terrain := StrToInt(Fields[9]);
            Node.ObjectId := StrToInt(Fields[10]);
            Node.Contenance := StrToInt(Fields[11]);

            // Ajouter au tableau
            SetLength(Nodes, Length(Nodes) + 1);
            Nodes[High(Nodes)] := Node;

          except
            on E: Exception do
              ; // WriteLn('ATTENTION: Erreur parsing ligne ', LineNum, ' des nodes: ', E.Message);
          end;
        end;
      finally
        Fields.Free;
      end;
    end;

    CloseFile(FileHandle);
    // WriteLn('Nodes chargés: ', Length(Nodes));
    Result := True;

  except
    on E: Exception do
      ; // WriteLn('ERREUR critique lors du chargement des nodes: ', E.Message);
  end;
end;

// Parse le fichier voie.csv et remplit le tableau de voies
function ParseEdgesCSV(const FilePath: String; out Edges: TEdgeArray): Boolean;
var
  FileHandle: TextFile;
  Line: String;
  Fields: TStringList;
  Edge: TEdge;
  LineNum: Integer;
  R, G, B, A: Byte;
begin
  Result := False;
  SetLength(Edges, 0);

  if not SysUtils.FileExists(FilePath) then
  begin
    // WriteLn('ERREUR: Fichier edges introuvable: ', FilePath);
    Exit;
  end;

  try
    AssignFile(FileHandle, FilePath);
    Reset(FileHandle);
    LineNum := 0;

    while not Eof(FileHandle) do
    begin
      ReadLn(FileHandle, Line);
      Inc(LineNum);

      // Ignorer la ligne d'en-tête
      if LineNum = 1 then
        Continue;

      // Ignorer les lignes vides
      if Trim(Line) = '' then
        Continue;

      Fields := SplitCSV(Line, ';');
      try
        // Vérifier qu'on a au moins 17 champs (13 données + 4 couleur RGBA)
        if Fields.Count >= 17 then
        begin
          try
            // Parser les données
            Edge.ID := StrToInt(Fields[0]);
            Edge.NodeA := StrToInt(Fields[1]);
            Edge.NodeB := StrToInt(Fields[2]);
            Edge.HandleX := StrToInt(Fields[3]);
            Edge.HandleY := StrToInt(Fields[4]);
            Edge.TypeVoie := StrToInt(Fields[5]);
            Edge.Direction := StrToInt(Fields[6]);
            Edge.Vitesse := StrToInt(Fields[7]);
            Edge.Tonnage := StrToInt(Fields[8]);
            Edge.Vehicules := StrToInt(Fields[9]);
            Edge.PoidsMax := StrToInt(Fields[10]);
            Edge.NatureSol := StrToInt(Fields[11]);
            Edge.Viabilite := StrToInt(Fields[12]);

            // Parser la couleur (4 dernières colonnes)
            R := StrToInt(Fields[13]);
            G := StrToInt(Fields[14]);
            B := StrToInt(Fields[15]);
            A := StrToInt(Fields[16]);
            Edge.Color := ColorCreate(R, G, B, A);

            // Ajouter au tableau
            SetLength(Edges, Length(Edges) + 1);
            Edges[High(Edges)] := Edge;

          except
            on E: Exception do
              ; // WriteLn('ATTENTION: Erreur parsing ligne ', LineNum, ' des edges: ', E.Message);
          end;
        end;
      finally
        Fields.Free;
      end;
    end;

    CloseFile(FileHandle);
    // WriteLn('Voies chargées: ', Length(Edges));
    Result := True;

  except
    on E: Exception do
      ; // WriteLn('ERREUR critique lors du chargement des voies: ', E.Message);
  end;
end;

// === FONCTION PUBLIQUE PRINCIPALE ===

function LoadGameData(const ProjectName: String;
                      out Texture: TTexture2D;
                      out Nodes: TNodeArray;
                      out Edges: TEdgeArray): Boolean;
var
  BasePath: String;
  ImagePath: String;
  NodesPath: String;
  EdgesPath: String;
  Image: TImage;
  Success: Boolean;
begin
  Result := False;
  Success := True;

  // Construire les chemins (Windows - backslash)
  BasePath := SAVED_DIR + '\' + ProjectName;
  ImagePath := BasePath + '\' + ProjectName + FILE_EXT_IMAGE;
  NodesPath := BasePath + '\' + FILE_EXT_NODES;
  EdgesPath := BasePath + '\' + FILE_EXT_EDGES;

  // Charger l'image de la carte
  if SysUtils.FileExists(ImagePath) then
  begin
    // WriteLn('Chargement de l''image: ', ImagePath);
    Image := LoadImage(PChar(ImagePath));
    Texture := LoadTextureFromImage(Image);
    UnloadImage(Image);
    // WriteLn('Texture chargée: ', Texture.width, 'x', Texture.height, ' pixels');
  end
  else
  begin
    // WriteLn('ERREUR: Image manquante: ', ImagePath);
    Success := False;
  end;

  // Charger les nœuds
  if not ParseNodesCSV(NodesPath, Nodes) then
  begin
    // WriteLn('ERREUR: Impossible de charger les nodes');
    Success := False;
  end;

  // Charger les voies
  if not ParseEdgesCSV(EdgesPath, Edges) then
  begin
    // WriteLn('ERREUR: Impossible de charger les edges');
    Success := False;
  end;

  // WriteLn('=== FIN CHARGEMENT ===');
  // WriteLn('');

  Result := Success;
end;

end.
