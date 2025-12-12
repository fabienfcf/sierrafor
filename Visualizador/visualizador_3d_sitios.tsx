import React, { useEffect, useRef, useState } from 'react';
import * as THREE from 'three';

// Datos de todos los sitios (extraídos de F03)
const SITIOS_DATA = {
  0: [{id:1,esp:81,dn:34,h:6,hc:0,x:0,y:0,rc:2.75},{id:2,esp:21,dn:19,h:7,hc:2.5,x:-2.88,y:-0.83,rc:2},{id:3,esp:62,dn:18,h:2,hc:0,x:0.67,y:-3.94,rc:0.5},{id:4,esp:62,dn:24,h:6,hc:4,x:1.73,y:-4.7,rc:0.5},{id:5,esp:62,dn:19,h:6,hc:3,x:2.05,y:-5.63,rc:1.75}],
  1: [{id:1,esp:21,dn:24,h:10,hc:3.5,x:0,y:0,rc:2.5},{id:2,esp:61,dn:17,h:4,hc:0,x:2.73,y:1.45,rc:1.5},{id:3,esp:62,dn:18,h:6,hc:2.5,x:-0.52,y:3,rc:1.5},{id:4,esp:62,dn:37,h:11,hc:3.5,x:5.12,y:2.96,rc:4},{id:5,esp:62,dn:21,h:8,hc:3,x:5.66,y:4.09,rc:2}],
  35: [{id:1,esp:63,dn:22,h:9,hc:4,x:0,y:0,rc:2.75},{id:2,esp:63,dn:7.5,h:6,hc:0,x:1.59,y:-2.54,rc:1.25},{id:3,esp:63,dn:7.5,h:7,hc:0,x:-1.08,y:-2.8,rc:1.25},{id:4,esp:62,dn:17.5,h:7,hc:4,x:2.88,y:-3.2,rc:2},{id:5,esp:62,dn:17.5,h:8,hc:2,x:3.54,y:-4.21,rc:2.75}],
  52: [{id:1,esp:11,dn:45,h:19,hc:12,x:0,y:0,rc:5},{id:2,esp:11,dn:32,h:16,hc:8,x:6.79,y:3.92,rc:4},{id:3,esp:62,dn:21,h:8,hc:3,x:-0.37,y:6.99,rc:2.25},{id:4,esp:62,dn:25,h:9,hc:4,x:-4.1,y:8.55,rc:3},{id:5,esp:62,dn:21,h:8,hc:3,x:-7.15,y:9.78,rc:2.5}]
};

const COLORES_ESPECIES = {
  11: '#2d5016', // Verde oscuro
  21: '#4a7c59', // Verde medio
  61: '#6b8e23', // Olivo
  62: '#8fbc8f', // Verde claro
  63: '#3cb371', // Verde mar
  65: '#228b22', // Verde bosque
  81: '#556b2f', // Olivo oscuro
  101: '#90ee90' // Verde pálido
};

const VisualizadorSitio = () => {
  const mountRef = useRef(null);
  const [sitioActual, setSitioActual] = useState(35);
  const [arbolSeleccionado, setArbolSeleccionado] = useState(null);
  const [vistaActual, setVistaActual] = useState('perspectiva');
  const [mostrarCopas, setMostrarCopas] = useState(true);
  const sceneRef = useRef(null);
  const cameraRef = useRef(null);
  const rendererRef = useRef(null);

  useEffect(() => {
    if (!mountRef.current) return;

    // Configuración de la escena
    const scene = new THREE.Scene();
    scene.background = new THREE.Color(0x87ceeb);
    sceneRef.current = scene;

    // Cámara
    const camera = new THREE.PerspectiveCamera(
      60,
      mountRef.current.clientWidth / mountRef.current.clientHeight,
      0.1,
      1000
    );
    camera.position.set(20, 20, 20);
    camera.lookAt(0, 0, 0);
    cameraRef.current = camera;

    // Renderer
    const renderer = new THREE.WebGLRenderer({ antialias: true });
    renderer.setSize(mountRef.current.clientWidth, mountRef.current.clientHeight);
    renderer.shadowMap.enabled = true;
    mountRef.current.appendChild(renderer.domElement);
    rendererRef.current = renderer;

    // Iluminación
    const ambientLight = new THREE.AmbientLight(0xffffff, 0.6);
    scene.add(ambientLight);

    const directionalLight = new THREE.DirectionalLight(0xffffff, 0.8);
    directionalLight.position.set(10, 20, 10);
    directionalLight.castShadow = true;
    scene.add(directionalLight);

    // Suelo
    const groundGeometry = new THREE.CircleGeometry(25, 32);
    const groundMaterial = new THREE.MeshLambertMaterial({ 
      color: 0x7cfc00,
      side: THREE.DoubleSide 
    });
    const ground = new THREE.Mesh(groundGeometry, groundMaterial);
    ground.rotation.x = -Math.PI / 2;
    ground.receiveShadow = true;
    scene.add(ground);

    // Grid de referencia
    const gridHelper = new THREE.GridHelper(40, 20, 0x888888, 0xcccccc);
    scene.add(gridHelper);

    // Ejes de referencia
    const axesHelper = new THREE.AxesHelper(15);
    scene.add(axesHelper);

    // Función para crear árbol
    const crearArbol = (arbol) => {
      const grupo = new THREE.Group();
      grupo.userData = arbol;

      // Tronco
      const radioTronco = arbol.dn / 200; // DN en metros
      const alturaTronco = arbol.hc > 0 ? arbol.hc : arbol.h * 0.4;
      const troncoGeometry = new THREE.CylinderGeometry(
        radioTronco, 
        radioTronco * 1.2, 
        alturaTronco, 
        8
      );
      const troncoMaterial = new THREE.MeshLambertMaterial({ color: 0x654321 });
      const tronco = new THREE.Mesh(troncoGeometry, troncoMaterial);
      tronco.position.y = alturaTronco / 2;
      tronco.castShadow = true;
      grupo.add(tronco);

      // Copa (esfera)
      if (mostrarCopas && arbol.rc > 0.1) {
        const longitudCopa = arbol.h - arbol.hc;
        const copaGeometry = new THREE.SphereGeometry(arbol.rc, 16, 16);
        const color = COLORES_ESPECIES[arbol.esp] || '#228b22';
        const copaMaterial = new THREE.MeshLambertMaterial({ 
          color: color,
          transparent: true,
          opacity: 0.8
        });
        const copa = new THREE.Mesh(copaGeometry, copaMaterial);
        copa.position.y = arbol.hc + longitudCopa / 2;
        copa.castShadow = true;
        grupo.add(copa);

        // Alambre de copa para ver traslapes
        const copaWireframe = new THREE.EdgesGeometry(copaGeometry);
        const copaLine = new THREE.LineSegments(
          copaWireframe,
          new THREE.LineBasicMaterial({ color: 0x004400, transparent: true, opacity: 0.3 })
        );
        copaLine.position.copy(copa.position);
        grupo.add(copaLine);
      }

      // Marcador en el suelo (posición XY)
      const marcadorGeometry = new THREE.CircleGeometry(0.3, 8);
      const marcadorMaterial = new THREE.MeshBasicMaterial({ 
        color: 0xff0000,
        side: THREE.DoubleSide
      });
      const marcador = new THREE.Mesh(marcadorGeometry, marcadorMaterial);
      marcador.rotation.x = -Math.PI / 2;
      marcador.position.y = 0.01;
      grupo.add(marcador);

      // Etiqueta de ID
      const canvas = document.createElement('canvas');
      const context = canvas.getContext('2d');
      canvas.width = 64;
      canvas.height = 64;
      context.fillStyle = 'white';
      context.font = 'Bold 40px Arial';
      context.textAlign = 'center';
      context.fillText(arbol.id.toString(), 32, 45);
      
      const texture = new THREE.CanvasTexture(canvas);
      const spriteMaterial = new THREE.SpriteMaterial({ map: texture });
      const sprite = new THREE.Sprite(spriteMaterial);
      sprite.position.y = arbol.h + 1;
      sprite.scale.set(1.5, 1.5, 1);
      grupo.add(sprite);

      grupo.position.set(arbol.x, 0, -arbol.y); // Z negativo para coordenada Y
      return grupo;
    };

    // Cargar sitio
    const cargarSitio = () => {
      // Limpiar árboles previos
      scene.children = scene.children.filter(
        child => !(child instanceof THREE.Group && child.userData.id)
      );

      const arboles = SITIOS_DATA[sitioActual] || [];
      arboles.forEach(arbol => {
        const arbolMesh = crearArbol(arbol);
        scene.add(arbolMesh);
      });
    };

    cargarSitio();

    // Control de cámara manual simplificado
    let isDragging = false;
    let previousMousePosition = { x: 0, y: 0 };
    let cameraAngle = 0;
    let cameraHeight = 20;
    let cameraDistance = 25;

    const updateCamera = () => {
      const x = cameraDistance * Math.cos(cameraAngle);
      const z = cameraDistance * Math.sin(cameraAngle);
      camera.position.set(x, cameraHeight, z);
      camera.lookAt(0, 5, 0);
    };

    const onMouseDown = (e) => {
      isDragging = true;
      previousMousePosition = { x: e.clientX, y: e.clientY };
    };

    const onMouseMove = (e) => {
      if (!isDragging) return;
      
      const deltaX = e.clientX - previousMousePosition.x;
      const deltaY = e.clientY - previousMousePosition.y;
      
      cameraAngle += deltaX * 0.01;
      cameraHeight = Math.max(5, Math.min(40, cameraHeight - deltaY * 0.1));
      
      updateCamera();
      previousMousePosition = { x: e.clientX, y: e.clientY };
    };

    const onMouseUp = () => {
      isDragging = false;
    };

    const onWheel = (e) => {
      e.preventDefault();
      cameraDistance = Math.max(10, Math.min(50, cameraDistance + e.deltaY * 0.01));
      updateCamera();
    };

    renderer.domElement.addEventListener('mousedown', onMouseDown);
    renderer.domElement.addEventListener('mousemove', onMouseMove);
    renderer.domElement.addEventListener('mouseup', onMouseUp);
    renderer.domElement.addEventListener('wheel', onWheel);

    // Click para seleccionar árbol
    const raycaster = new THREE.Raycaster();
    const mouse = new THREE.Vector2();

    const onMouseClick = (e) => {
      const rect = renderer.domElement.getBoundingClientRect();
      mouse.x = ((e.clientX - rect.left) / rect.width) * 2 - 1;
      mouse.y = -((e.clientY - rect.top) / rect.height) * 2 + 1;

      raycaster.setFromCamera(mouse, camera);
      const intersects = raycaster.intersectObjects(scene.children, true);

      if (intersects.length > 0) {
        let grupo = intersects[0].object;
        while (grupo.parent && !(grupo.userData.id)) {
          grupo = grupo.parent;
        }
        if (grupo.userData.id) {
          setArbolSeleccionado(grupo.userData);
        }
      }
    };

    renderer.domElement.addEventListener('click', onMouseClick);

    // Animación
    const animate = () => {
      requestAnimationFrame(animate);
      renderer.render(scene, camera);
    };
    animate();

    // Cleanup
    return () => {
      if (mountRef.current && renderer.domElement) {
        mountRef.current.removeChild(renderer.domElement);
      }
      renderer.domElement.removeEventListener('mousedown', onMouseDown);
      renderer.domElement.removeEventListener('mousemove', onMouseMove);
      renderer.domElement.removeEventListener('mouseup', onMouseUp);
      renderer.domElement.removeEventListener('wheel', onWheel);
      renderer.domElement.removeEventListener('click', onMouseClick);
    };
  }, [sitioActual, mostrarCopas]);

  // Cambiar vista de cámara
  useEffect(() => {
    if (!cameraRef.current) return;
    
    const camera = cameraRef.current;
    switch(vistaActual) {
      case 'cenital':
        camera.position.set(0, 40, 0);
        camera.lookAt(0, 0, 0);
        break;
      case 'lateral':
        camera.position.set(30, 10, 0);
        camera.lookAt(0, 5, 0);
        break;
      default:
        camera.position.set(20, 20, 20);
        camera.lookAt(0, 5, 0);
    }
  }, [vistaActual]);

  const sitiosDisponibles = Object.keys(SITIOS_DATA).map(Number);
  const arbolesActuales = SITIOS_DATA[sitioActual] || [];
  
  // Calcular métricas del sitio
  const areaTotalCopas = arbolesActuales.reduce((sum, a) => sum + Math.PI * a.rc * a.rc, 0);
  const radioSitio = Math.max(...arbolesActuales.map(a => 
    Math.sqrt(a.x*a.x + a.y*a.y) + a.rc
  ));
  const areaSitio = Math.PI * radioSitio * radioSitio;
  const ccf = (areaTotalCopas / areaSitio) * 100;

  return (
    <div className="w-full h-screen flex flex-col bg-gray-100">
      {/* Barra de control */}
      <div className="bg-green-800 text-white p-4 shadow-lg">
        <div className="flex items-center justify-between gap-4">
          <div className="flex items-center gap-4">
            <h1 className="text-xl font-bold">Visualizador 3D - Sitios Forestales</h1>
            <select 
              value={sitioActual} 
              onChange={(e) => setSitioActual(Number(e.target.value))}
              className="bg-white text-black px-3 py-2 rounded"
            >
              {sitiosDisponibles.map(s => (
                <option key={s} value={s}>Sitio {s}</option>
              ))}
            </select>
          </div>
          
          <div className="flex gap-2">
            <button 
              onClick={() => setVistaActual('perspectiva')}
              className={`px-3 py-2 rounded ${vistaActual === 'perspectiva' ? 'bg-green-600' : 'bg-green-700'}`}
            >
              Perspectiva
            </button>
            <button 
              onClick={() => setVistaActual('cenital')}
              className={`px-3 py-2 rounded ${vistaActual === 'cenital' ? 'bg-green-600' : 'bg-green-700'}`}
            >
              Cenital
            </button>
            <button 
              onClick={() => setVistaActual('lateral')}
              className={`px-3 py-2 rounded ${vistaActual === 'lateral' ? 'bg-green-600' : 'bg-green-700'}`}
            >
              Lateral
            </button>
            <button 
              onClick={() => setMostrarCopas(!mostrarCopas)}
              className="px-3 py-2 rounded bg-green-700"
            >
              {mostrarCopas ? 'Ocultar' : 'Mostrar'} Copas
            </button>
          </div>
        </div>
      </div>

      <div className="flex flex-1 overflow-hidden">
        {/* Escena 3D */}
        <div ref={mountRef} className="flex-1" />

        {/* Panel de información */}
        <div className="w-80 bg-white p-4 overflow-y-auto shadow-lg">
          <h2 className="text-lg font-bold mb-3 text-green-800">Sitio {sitioActual}</h2>
          
          <div className="bg-green-50 p-3 rounded mb-4">
            <p className="text-sm mb-1"><span className="font-semibold">Árboles:</span> {arbolesActuales.length}</p>
            <p className="text-sm mb-1"><span className="font-semibold">CCF:</span> {ccf.toFixed(1)}%</p>
            <p className="text-sm mb-1">
              <span className="font-semibold">Estado:</span>{' '}
              <span className={ccf < 40 ? 'text-orange-600' : ccf > 70 ? 'text-red-600' : 'text-green-600'}>
                {ccf < 40 ? 'Subpoblado' : ccf > 70 ? 'Sobrepoblado' : 'Óptimo'}
              </span>
            </p>
            <p className="text-sm"><span className="font-semibold">Radio sitio:</span> {radioSitio.toFixed(1)}m</p>
          </div>

          {arbolSeleccionado && (
            <div className="bg-blue-50 p-3 rounded mb-4 border-2 border-blue-400">
              <h3 className="font-bold text-blue-800 mb-2">Árbol Seleccionado #{arbolSeleccionado.id}</h3>
              <p className="text-sm mb-1"><span className="font-semibold">Especie:</span> {arbolSeleccionado.esp}</p>
              <p className="text-sm mb-1"><span className="font-semibold">DN:</span> {arbolSeleccionado.dn} cm</p>
              <p className="text-sm mb-1"><span className="font-semibold">Altura:</span> {arbolSeleccionado.h} m</p>
              <p className="text-sm mb-1"><span className="font-semibold">Alt. copa:</span> {arbolSeleccionado.hc} m</p>
              <p className="text-sm mb-1"><span className="font-semibold">Radio copa:</span> {arbolSeleccionado.rc.toFixed(2)} m</p>
              <p className="text-sm mb-1"><span className="font-semibold">Posición:</span> ({arbolSeleccionado.x.toFixed(1)}, {arbolSeleccionado.y.toFixed(1)})</p>
            </div>
          )}

          <h3 className="font-bold mb-2">Lista de Árboles</h3>
          <div className="space-y-2">
            {arbolesActuales.map(a => (
              <div 
                key={a.id}
                className="p-2 bg-gray-50 rounded hover:bg-gray-100 cursor-pointer text-sm"
                onClick={() => setArbolSeleccionado(a)}
              >
                <div className="flex items-center gap-2">
                  <div 
                    className="w-4 h-4 rounded-full"
                    style={{ backgroundColor: COLORES_ESPECIES[a.esp] || '#228b22' }}
                  />
                  <span className="font-semibold">#{a.id}</span>
                  <span className="text-gray-600">DN:{a.dn}cm H:{a.h}m</span>
                </div>
              </div>
            ))}
          </div>

          <div className="mt-4 p-3 bg-gray-50 rounded text-xs text-gray-600">
            <p className="font-semibold mb-1">Controles:</p>
            <p>• Arrastrar: rotar vista</p>
            <p>• Scroll: zoom</p>
            <p>• Click: seleccionar árbol</p>
          </div>
        </div>
      </div>
    </div>
  );
};

export default VisualizadorSitio;
