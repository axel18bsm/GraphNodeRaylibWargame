{*******************************************************************************
  GameUI.pas
  Interface Utilisateur pour le Wargame Viewer
  
  Gère l'affichage des panneaux UI :
  - Panneau droit : Informations sur l'objet sélectionné
  - Panneau bas : Logs système
  
  Utilise Raygui pour un rendu professionnel
*******************************************************************************}
{$mode objfpc}{$H+}

unit GameUI;

interface

uses
  SysUtils,
  raylib,
  raygui,
  GameConfig,
  GameTypes;

type
  // Référence à un objet sélectionné (peut être un Node ou un Edge)
  TSelectedObject = record
    IsNode: Boolean;       // True si c'est un Node, False si c'est un Edge
    NodeIndex: Integer;    // Index dans le tableau Nodes (si IsNode = True)
    EdgeIndex: Integer;    // Index dans le tableau Edges (si IsNode = False)
  end;

// Dessine l'interface utilisateur complète
procedure DrawGameUI(const Nodes: TNodeArray;
                     const Edges: TEdgeArray;
                     const Selected: TSelectedObject;
                     const SelectedValid: Boolean;
                     const LogMessage: String);

implementation

// Fonction helper pour dessiner une paire clé-valeur
procedure DrawKeyValue(X, Y: Integer; const Key, Value: String);
begin
  DrawText(PChar(Key + ':'), X, Y, 18, GRAY);
  DrawText(PChar(Value), X + 110, Y, 18, WHITE);
end;

// Fonction helper pour dessiner un label de titre
procedure DrawLabelTitle(X, Y: Integer; const Title: String; Color: TColor);
begin
  DrawText(PChar(Title), X, Y, 20, Color);
end;

// Dessine le panneau droit avec les informations de l'objet sélectionné
procedure DrawRightPanel(const Nodes: TNodeArray;
                         const Edges: TEdgeArray;
                         const Selected: TSelectedObject;
                         const SelectedValid: Boolean);
var
  PanelRect: TRectangle;
  X, Y: Integer;
  Spacing: Integer;
  Node: TNode;
  Edge: TEdge;
begin
  // Définir le rectangle du panneau droit
  PanelRect := RectangleCreate(VIEW_WIDTH, 0, PANEL_RIGHT_WIDTH, SCREEN_HEIGHT);
  
  // Dessiner le fond du panneau avec Raygui
  GuiPanel(PanelRect, PChar('INFORMATIONS'));
  
  X := Trunc(PanelRect.x) + 15;
  Y := 50;
  Spacing := 25;
  
  // Si aucun objet sélectionné
  if not SelectedValid then
  begin
    DrawText(PChar('Cliquez sur un element'), X, Y, 18, DARKGRAY);
    DrawText(PChar('pour voir ses details.'), X, Y + 25, 18, DARKGRAY);
    Exit;
  end;
  
  // Si c'est un Node (Ville/Nœud)
  if Selected.IsNode then
  begin
    if (Selected.NodeIndex >= 0) and (Selected.NodeIndex < Length(Nodes)) then
    begin
      Node := Nodes[Selected.NodeIndex];
      
      DrawLabelTitle(X, Y, 'VILLE (NOEUD)', HIGHLIGHT_COLOR);
      Inc(Y, 40);
      
      DrawKeyValue(X, Y, 'ID', IntToStr(Node.ID));
      Inc(Y, Spacing);
      
      DrawKeyValue(X, Y, 'Nom', Node.Name);
      Inc(Y, Spacing);
      
      DrawKeyValue(X, Y, 'Position', Format('%d, %d', [Node.X, Node.Y]));
      Inc(Y, Spacing + 10);
      
      DrawKeyValue(X, Y, 'Pts Victoire', IntToStr(Node.VP));
      Inc(Y, Spacing);
      
      DrawKeyValue(X, Y, 'Hauteur', IntToStr(Node.Height));
      Inc(Y, Spacing);
      
      DrawKeyValue(X, Y, 'Type Terrain', IntToStr(Node.Terrain));
      Inc(Y, Spacing);
      
      DrawKeyValue(X, Y, 'Type Objet', IntToStr(Node.ObjectId));
      Inc(Y, Spacing);
      
      DrawKeyValue(X, Y, 'Contenance', IntToStr(Node.Contenance));
      Inc(Y, Spacing);
    end;
  end
  // Si c'est un Edge (Voie/Trajet)
  else
  begin
    if (Selected.EdgeIndex >= 0) and (Selected.EdgeIndex < Length(Edges)) then
    begin
      Edge := Edges[Selected.EdgeIndex];
      
      DrawLabelTitle(X, Y, 'VOIE (TRAJET)', HIGHLIGHT_COLOR);
      Inc(Y, 40);
      
      DrawKeyValue(X, Y, 'ID', IntToStr(Edge.ID));
      Inc(Y, Spacing);
      
      DrawKeyValue(X, Y, 'De Node', IntToStr(Edge.NodeA));
      Inc(Y, Spacing);
      
      DrawKeyValue(X, Y, 'Vers Node', IntToStr(Edge.NodeB));
      Inc(Y, Spacing + 10);
      
      DrawKeyValue(X, Y, 'Type Voie', IntToStr(Edge.TypeVoie));
      Inc(Y, Spacing);
      
      DrawKeyValue(X, Y, 'Cout (Vit)', IntToStr(Edge.Vitesse));
      Inc(Y, Spacing);
      
      DrawKeyValue(X, Y, 'Direction', IntToStr(Edge.Direction));
      Inc(Y, Spacing);
      
      DrawKeyValue(X, Y, 'Tonnage', IntToStr(Edge.Tonnage));
      Inc(Y, Spacing);
      
      DrawKeyValue(X, Y, 'Vehicules', IntToStr(Edge.Vehicules));
      Inc(Y, Spacing);
      
      DrawKeyValue(X, Y, 'Poids Max', IntToStr(Edge.PoidsMax));
      Inc(Y, Spacing);
      
      DrawKeyValue(X, Y, 'Nature Sol', IntToStr(Edge.NatureSol));
      Inc(Y, Spacing);
      
      DrawKeyValue(X, Y, 'Viabilite', IntToStr(Edge.Viabilite));
      Inc(Y, Spacing);
    end;
  end;
end;

// Dessine le panneau bas avec les logs système
procedure DrawBottomPanel(const LogMessage: String);
var
  PanelRect: TRectangle;
begin
  // Définir le rectangle du panneau bas
  PanelRect := RectangleCreate(0, VIEW_HEIGHT, VIEW_WIDTH, PANEL_BOTTOM_HEIGHT);
  
  // Dessiner le fond du panneau avec Raygui
  GuiPanel(PanelRect, PChar('LOGS SYSTEME'));
  
  // Dessiner le message de log
  DrawText(PChar(LogMessage), 20, Trunc(PanelRect.y) + 40, 20, LOG_COLOR);
end;

// Fonction principale publique
procedure DrawGameUI(const Nodes: TNodeArray;
                     const Edges: TEdgeArray;
                     const Selected: TSelectedObject;
                     const SelectedValid: Boolean;
                     const LogMessage: String);
begin
  // Dessiner les deux panneaux
  DrawRightPanel(Nodes, Edges, Selected, SelectedValid);
  DrawBottomPanel(LogMessage);
end;

end.
