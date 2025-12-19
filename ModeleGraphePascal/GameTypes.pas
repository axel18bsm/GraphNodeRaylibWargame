{*******************************************************************************
  GameTypes.pas
  Structures de données et types pour le Wargame Viewer
  
  Définit :
  - TNode : Représente un nœud/ville sur la carte
  - TEdge : Représente une voie/trajet entre deux nœuds
  - Arrays dynamiques pour stocker les nœuds et voies
  - Fonctions utilitaires (Clamp, création de vecteurs, etc.)
*******************************************************************************}
{$mode objfpc}{$H+}

unit GameTypes;

interface

uses
  raylib,
  GameConfig;

type
  // === NOEUD (Ville/Position sur la carte) ===
  TNode = record
    ID: Integer;              // Identifiant unique du nœud
    X, Y: Integer;            // Position en pixels sur la carte
    Shape: Integer;           // Forme : 0=Cercle, 1=Rectangle
    Dim1, Dim2: Integer;      // Dimensions (Rayon pour cercle, Largeur/Hauteur pour rect)
    Name: String;             // Nom de la ville/localité
    VP: Integer;              // Points de Victoire
    Height: Integer;          // Altitude/Hauteur
    Terrain: Integer;         // Type de terrain
    ObjectId: Integer;        // Type d'objet présent
    Contenance: Integer;      // Capacité/Contenance
  end;
  
  // === VOIE (Trajet entre deux nœuds) ===
  TEdge = record
    ID: Integer;              // Identifiant unique de la voie
    NodeA, NodeB: Integer;    // IDs des nœuds reliés (début et fin)
    HandleX, HandleY: Integer; // Position de la poignée (handle) pour l'affichage
    TypeVoie: Integer;        // Type de voie (route, chemin de fer, etc.)
    Direction: Integer;       // Direction (aller/retour/bidirectionnel)
    Vitesse: Integer;         // Vitesse (utilisée comme coût de déplacement)
    Tonnage: Integer;         // Capacité en tonnage
    Vehicules: Integer;       // Nombre de véhicules
    PoidsMax: Integer;        // Poids maximum supporté
    NatureSol: Integer;       // Nature du sol
    Viabilite: Integer;       // État de viabilité
    Color: TColor;            // Couleur d'affichage de la voie
  end;

  // === ARRAYS DYNAMIQUES ===
  TNodeArray = array of TNode;
  TEdgeArray = array of TEdge;

// === FONCTIONS UTILITAIRES ===

// Limite une valeur entre un minimum et un maximum
function Clamp(Value, Min, Max: Single): Single;

// Crée un vecteur 2D à partir de deux coordonnées
function Vector2Create(X, Y: Single): TVector2;

// Crée un rectangle à partir de coordonnées et dimensions
function RectangleCreate(X, Y, Width, Height: Single): TRectangle;

// Crée une couleur à partir de composantes RGBA
function ColorCreate(R, G, B, A: Byte): TColor;

implementation

// Fonction Clamp : Limite une valeur dans un intervalle
function Clamp(Value, Min, Max: Single): Single;
begin
  if Value < Min then
    Result := Min
  else if Value > Max then
    Result := Max
  else
    Result := Value;
end;

// Crée un TVector2
function Vector2Create(X, Y: Single): TVector2;
begin
  Result.x := X;
  Result.y := Y;
end;

// Crée un TRectangle
function RectangleCreate(X, Y, Width, Height: Single): TRectangle;
begin
  Result.x := X;
  Result.y := Y;
  Result.width := Width;
  Result.height := Height;
end;

// Crée une TColor
function ColorCreate(R, G, B, A: Byte): TColor;
begin
  Result.r := R;
  Result.g := G;
  Result.b := B;
  Result.a := A;
end;

end.
