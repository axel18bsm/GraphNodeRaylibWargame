{*******************************************************************************
  GameConfig.pas
  Configuration et Constantes pour le Wargame Viewer
  
  Contient toutes les constantes globales du projet :
  - Dimensions de la fenêtre et zones UI
  - Chemins de fichiers
  - Couleurs personnalisées
  - Paramètres de gameplay
*******************************************************************************}
{$mode objfpc}{$H+}

unit GameConfig;

interface

uses
  raylib;

const
  // === FENETRE PRINCIPALE ===
  SCREEN_WIDTH = 1600;
  SCREEN_HEIGHT = 1024;
  WINDOW_TITLE = 'Wargame Model - Viewer';
  TARGET_FPS = 60;

  // === LAYOUT UI ===
  // Panneau droit pour afficher les informations de l'objet sélectionné
  PANEL_RIGHT_WIDTH = 300;
  // Panneau bas pour afficher les logs système
  PANEL_BOTTOM_HEIGHT = 150;
  
  // Zone de vue (carte) = Écran total - Panneaux UI
  VIEW_WIDTH = SCREEN_WIDTH - PANEL_RIGHT_WIDTH;      // 1300 pixels
  VIEW_HEIGHT = SCREEN_HEIGHT - PANEL_BOTTOM_HEIGHT;  // 874 pixels

  // === CHEMINS DE FICHIERS ===
  RESOURCE_DIR = 'Ressource';
  SAVED_DIR = 'saved';
  DEFAULT_PROJECT = 'CarteNord1870';  // Nom du projet par défaut à charger
  
  // Extensions de fichiers attendues
  FILE_EXT_IMAGE = '.png';
  FILE_EXT_NODES = 'dataGraph.csv';
  FILE_EXT_EDGES = 'voie.csv';

  // === COULEURS PERSONNALISEES ===
  // Couleur de fond général
  BG_COLOR: TColor = (r: 0; g: 82; b: 172; a: 255);  // DARKBLUE
  
  // Couleurs pour l'UI
  UI_BG_COLOR: TColor = (r: 0; g: 0; b: 0; a: 255);        // BLACK
  UI_BORDER_COLOR: TColor = (r: 80; g: 80; b: 80; a: 255); // DARKGRAY
  TEXT_COLOR: TColor = (r: 255; g: 255; b: 255; a: 255);   // WHITE
  
  // Couleur de surbrillance pour les objets sélectionnés
  HIGHLIGHT_COLOR: TColor = (r: 255; g: 161; b: 0; a: 255); // ORANGE
  
  // Couleurs pour les logs et coordonnées
  LOG_COLOR: TColor = (r: 0; g: 228; b: 48; a: 255);    // GREEN
  COORD_COLOR: TColor = (r: 230; g: 41; b: 55; a: 255); // RED

  // === PARAMETRES DE GAMEPLAY ===
  // Taille des poignées (handles) des voies en pixels
  HANDLE_SIZE = 12;
  
  // Types de formes pour les nœuds
  SHAPE_CIRCLE = 0;  // Nœud de forme circulaire
  SHAPE_RECT = 1;    // Nœud de forme rectangulaire
  
  // Limites de zoom de la caméra
  ZOOM_MIN = 0.5;    // Zoom minimum (50% - on voit plus large)
  ZOOM_MAX = 3.0;    // Zoom maximum (300% - on voit plus détaillé)
  ZOOM_SPEED = 0.125; // Vitesse de zoom (facteur multiplicateur)

implementation

end.
