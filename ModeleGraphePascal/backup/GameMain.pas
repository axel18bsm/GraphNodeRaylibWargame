{*******************************************************************************
  GameMain.pas
  Programme Principal du Wargame Viewer
  
  Boucle de jeu principale qui :
  - Initialise la fenêtre et charge les données
  - Gère la caméra 2D (Zoom et Pan)
  - Gère les interactions souris (picking)
  - Affiche la carte, les voies et les nœuds
  - Affiche l'interface utilisateur
*******************************************************************************}
{$mode objfpc}{$H+}

program gamemain;

uses
  SysUtils,
  raylib,
  raygui,
  GameConfig,
  GameTypes,
  GameLoader,
  GameUI, raymath;

var
  // Données du jeu
  MapTexture: TTexture2D;
  Nodes: TNodeArray;
  Edges: TEdgeArray;
  
  // Caméra 2D pour navigation
  Camera: TCamera2D;
  
  // Sélection d'objet
  SelectedObject: TSelectedObject;
  SelectedValid: Boolean;
  
  // Message de log
  LogMessage: String;
  
  // Variables temporaires
  WheelMove: Single;
  MouseWorldBefore, MousePos, MouseWorld: TVector2;
  MouseDelta: TVector2;
  ScaleFactor: Single;
  WorldTopLeft, WorldBottomRight: TVector2;
  i: Integer;
  HalfSize: Single;
  Success: Boolean;

// === INITIALISATION ===

procedure InitializeCamera;
begin
  Camera.zoom := 1.0;
  Camera.target := Vector2Create(0, 0);
  Camera.offset := Vector2Create(0, 0);
  Camera.rotation := 0.0;
end;

// === GESTION DE LA CAMERA ===

procedure UpdateCamera;
begin
  // 1. GESTION DU ZOOM (Molette de la souris)
  WheelMove := GetMouseWheelMove();
  if WheelMove <> 0 then
  begin
    // Récupérer la position souris DANS LE MONDE avant de zoomer
    MouseWorldBefore := GetScreenToWorld2D(GetMousePosition(), Camera);
    
    // Configurer la caméra pour zoomer vers la souris
    Camera.offset := GetMousePosition();
    Camera.target := MouseWorldBefore;
    
    // Appliquer le zoom
    ScaleFactor := 1.0 + (ZOOM_SPEED * WheelMove);
    Camera.zoom := Clamp(Camera.zoom * ScaleFactor, ZOOM_MIN, ZOOM_MAX);
  end;
  
  // 2. GESTION DU PAN (Clic Gauche maintenu)
  if IsMouseButtonDown(MOUSE_BUTTON_LEFT) then
  begin
    MouseDelta := GetMouseDelta();
    // Inverser le delta et diviser par le zoom pour un mouvement naturel
    Camera.target.x := Camera.target.x - (MouseDelta.x / Camera.zoom);
    Camera.target.y := Camera.target.y - (MouseDelta.y / Camera.zoom);
  end;
  
  // 3. CLAMPING INTELLIGENT (Limites de la carte)
  if MapTexture.id > 0 then
  begin
    // Calculer où se trouvent les coins de l'écran dans le monde de la carte
    WorldTopLeft := GetScreenToWorld2D(Vector2Create(0, 0), Camera);
    WorldBottomRight := GetScreenToWorld2D(Vector2Create(VIEW_WIDTH, VIEW_HEIGHT), Camera);
    
    // Correction Gauche
    if WorldTopLeft.x < 0 then
      Camera.target.x := Camera.target.x + (0 - WorldTopLeft.x);
    
    // Correction Haut
    if WorldTopLeft.y < 0 then
      Camera.target.y := Camera.target.y + (0 - WorldTopLeft.y);
    
    // Correction Droite
    if WorldBottomRight.x > MapTexture.width then
    begin
      if (WorldBottomRight.x - WorldTopLeft.x) < MapTexture.width then
        Camera.target.x := Camera.target.x - (WorldBottomRight.x - MapTexture.width);
    end;
    
    // Correction Bas
    if WorldBottomRight.y > MapTexture.height then
    begin
      if (WorldBottomRight.y - WorldTopLeft.y) < MapTexture.height then
        Camera.target.y := Camera.target.y - (WorldBottomRight.y - MapTexture.height);
    end;
  end;
end;

// === DETECTION DE COLLISION (PICKING) ===

// Vérifie si la souris est sur une poignée de voie
function CheckHandleCollision(const Edge: TEdge; MouseX, MouseY: Single): Boolean;
var
  HandleRect: TRectangle;
  MousePoint: TVector2;
begin
  HalfSize := HANDLE_SIZE / 2.0;
  HandleRect := RectangleCreate(Edge.HandleX - HalfSize, Edge.HandleY - HalfSize, 
                                HANDLE_SIZE, HANDLE_SIZE);
  MousePoint := Vector2Create(MouseX, MouseY);
  Result := CheckCollisionPointRec(MousePoint, HandleRect);
end;

// Vérifie si la souris est sur un nœud
function CheckNodeCollision(const Node: TNode; MouseX, MouseY: Single): Boolean;
var
  MousePoint: TVector2;
  NodeRect: TRectangle;
begin
  MousePoint := Vector2Create(MouseX, MouseY);
  
  if Node.Shape = SHAPE_CIRCLE then
  begin
    // Collision avec un cercle
    Result := CheckCollisionPointCircle(MousePoint, 
                                       Vector2Create(Node.X, Node.Y), 
                                       Node.Dim1);
  end
  else
  begin
    // Collision avec un rectangle
    HalfSize := Node.Dim1 / 2.0;
    NodeRect := RectangleCreate(Node.X - HalfSize, Node.Y - (Node.Dim2 / 2.0), 
                                Node.Dim1, Node.Dim2);
    Result := CheckCollisionPointRec(MousePoint, NodeRect);
  end;
end;

// Trouve l'objet sous la souris (priorité aux poignées)
procedure FindObjectUnderMouse;
var
  i: Integer;
begin
  MousePos := GetMousePosition();
  
  // Vérifier que la souris est dans la zone de vue (pas dans les panneaux UI)
  if (MousePos.x > VIEW_WIDTH) or (MousePos.y > VIEW_HEIGHT) then
    Exit;
  
  // Convertir position souris vers coordonnées monde
  MouseWorld := GetScreenToWorld2D(MousePos, Camera);
  
  // Vérifier d'abord les poignées de voies (prioritaire)
  for i := High(Edges) downto 0 do
  begin
    if CheckHandleCollision(Edges[i], MouseWorld.x, MouseWorld.y) then
    begin
      SelectedObject.IsNode := False;
      SelectedObject.EdgeIndex := i;
      SelectedValid := True;
      LogMessage := Format('Selection : Voie ID %d', [Edges[i].ID]);
      Exit;
    end;
  end;
  
  // Ensuite vérifier les nœuds
  for i := High(Nodes) downto 0 do
  begin
    if CheckNodeCollision(Nodes[i], MouseWorld.x, MouseWorld.y) then
    begin
      SelectedObject.IsNode := True;
      SelectedObject.NodeIndex := i;
      SelectedValid := True;
      LogMessage := Format('Selection : %s (ID %d)', [Nodes[i].Name, Nodes[i].ID]);
      Exit;
    end;
  end;
end;

// === RENDU ===

procedure DrawMap;
begin
  if MapTexture.id > 0 then
  begin
    BeginMode2D(Camera);
    
    // Dessiner l'image de la carte
    DrawTexture(MapTexture, 0, 0, WHITE);
    
    // Dessiner un cadre rouge autour de la carte pour débug
    DrawRectangleLines(0, 0, MapTexture.width, MapTexture.height, RED);
    
    EndMode2D();
  end;
end;

procedure DrawEdges;
var
  i: Integer;
  Edge: TEdge;
  HalfSize: Single;
  HandleX, HandleY: Integer;
  IsSelected: Boolean;
begin
  BeginMode2D(Camera);
  
  HalfSize := HANDLE_SIZE / 2.0;
  
  for i := 0 to High(Edges) do
  begin
    Edge := Edges[i];
    HandleX := Edge.HandleX - Trunc(HalfSize);
    HandleY := Edge.HandleY - Trunc(HalfSize);
    
    // Vérifier si cette voie est sélectionnée
    IsSelected := SelectedValid and (not SelectedObject.IsNode) and 
                  (SelectedObject.EdgeIndex = i);
    
    if IsSelected then
    begin
      // Dessiner en surbrillance
      DrawRectangleRec(RectangleCreate(HandleX, HandleY, HANDLE_SIZE, HANDLE_SIZE), 
                      HIGHLIGHT_COLOR);
      DrawRectangleLinesEx(RectangleCreate(HandleX, HandleY, HANDLE_SIZE, HANDLE_SIZE), 
                          2, WHITE);
    end
    else
    begin
      // Dessiner normalement
      DrawRectangleRec(RectangleCreate(HandleX, HandleY, HANDLE_SIZE, HANDLE_SIZE), 
                      Edge.Color);
      DrawRectangleLines(HandleX, HandleY, HANDLE_SIZE, HANDLE_SIZE, WHITE);
    end;
  end;
  
  EndMode2D();
end;

procedure DrawNodes;
var

  Node: TNode;

  HalfW, HalfH: Single;
begin
  BeginMode2D(Camera);
  
  // On ne dessine que les nœuds sélectionnés (surbrillance)
  if SelectedValid and SelectedObject.IsNode then
  begin
    if (SelectedObject.NodeIndex >= 0) and (SelectedObject.NodeIndex < Length(Nodes)) then
    begin
      Node := Nodes[SelectedObject.NodeIndex];
      
      if Node.Shape = SHAPE_CIRCLE then
      begin
        DrawCircleLines(Node.X, Node.Y, Node.Dim1, HIGHLIGHT_COLOR);
      end
      else
      begin
        HalfW := Node.Dim1 / 2.0;
        HalfH := Node.Dim2 / 2.0;
        DrawRectangleLinesEx(RectangleCreate(Node.X - HalfW, Node.Y - HalfH, 
                            Node.Dim1, Node.Dim2), 2, HIGHLIGHT_COLOR);
      end;
    end;
  end;
  
  EndMode2D();
end;

// === BOUCLE PRINCIPALE ===

begin
  // Initialiser la fenêtre
  InitWindow(SCREEN_WIDTH, SCREEN_HEIGHT, PChar(WINDOW_TITLE));
  SetTargetFPS(TARGET_FPS);
  
  // Initialiser la caméra
  InitializeCamera();
  
  // Charger les données du jeu
 // WriteLn(Format('Chargement du projet : %s', [DEFAULT_PROJECT]));
  Success := LoadGameData(DEFAULT_PROJECT, MapTexture, Nodes, Edges);
  
  // Initialiser la sélection
  SelectedValid := False;
  SelectedObject.IsNode := False;
  SelectedObject.NodeIndex := -1;
  SelectedObject.EdgeIndex := -1;
  
  if Success then
    LogMessage := 'Projet charge avec succes.'
  else
    LogMessage := 'ERREUR lors du chargement du projet.';
  
  // === BOUCLE DE JEU ===
  while not WindowShouldClose() do
  begin
    // 1. MISE A JOUR
    UpdateCamera();
    
    // 2. DETECTION DE CLIC (Released = fin du clic)
    if IsMouseButtonReleased(MOUSE_BUTTON_LEFT) then
    begin
      FindObjectUnderMouse();
    end;
    
    // 3. RENDU
    BeginDrawing();
    
      ClearBackground(BG_COLOR);
      
      // Dessiner la carte et les éléments du jeu
      DrawMap();
      DrawEdges();
      DrawNodes();
      
      // Dessiner l'interface utilisateur par-dessus
      DrawGameUI(Nodes, Edges, SelectedObject, SelectedValid, LogMessage);
      
      // Afficher un message d'erreur si le chargement a échoué
      if not Success then
      begin
        DrawText(PChar('ERREUR CRITIQUE - Verifiez les fichiers'), 10, 10, 30, RED);
      end;
    
    EndDrawing();
  end;
  
  // === NETTOYAGE ===
  if MapTexture.id > 0 then
    UnloadTexture(MapTexture);
  
  CloseWindow();
end.
