# Examples.deepartist.org
There are a number of examples released with deepartist.org which attempt to illustrate the concepts of these techniques and the capabilities of this software. These examples are free to use as starting points for any of your art projects. Each example is published to the site as a full example report, with details, code, links, and further images in each.

## Textures & Core Concepts
The basic setup for a deep painting is that you have a canvas, which can be modified at will by the AI process, to match a particular signal. This signal matching network consists of a filter, which modifies the image into more abstract data, and then a loss function which measures how close that data signal is to a reference (i.e. style) signal. These filters and operators can then be scaled and combined to form a limitless variety.

### Pipelines and Layers
A pipeline is a pre-trained deep vision network which is loaded and made available as a series of image processing layers. Each layer builds on the last to extract higher-level details from the source image. This can be illustrated by this example which surveys the sequence of layers in the VGG19 pipeline, using each layer to reconstruct a target signal.

[![](http://examples.deepartist.org/img/df7795a9-c896-4d46-bef4-81cca8333d40.gif)](http://examples.deepartist.org/TextureSurvey/9e77dac3-fec8-4c7b-8be6-9df57044ec58/9e77dac3-fec8-4c7b-8be6-9df57044ec58.html)


### Operators - Signal Matchers and Enhancers
You can also configure operators, which define how close a signal is to the objective. This objective may be some kind of “similarity measure” to compute how close one image is to another, and many such measures exist; these approach zero. Others simply seek to increase a measure, for example rms “power” as used in DeepDream, possibly with some cutoff before infinity. A survey example of them displays the varied effects:

[![](http://examples.deepartist.org/img/f02159f3-2452-4d72-90e5-1ef001e97c05.gif)](http://examples.deepartist.org/OperatorSurvey/e670485f-9fba-4c72-8e23-c6fe10f4de00/e670485f-9fba-4c72-8e23-c6fe10f4de00.html)

### Seed Canvas - Noise, Plasma, Image
When a painting process is begun, there must be some data on the canvas to start with. Interestingly, a completely blank canvas produces poor results, due to something called “symmetry breaking”. There are three main types of seed used in deepartist.org: Noise, Plasma, or Image. Noise is simple random white noise, which can be scaled and offset. (For that matter, scaling and offset syntax is available for all image input urls) Plasma is a simple algorithmic texture that resembles a randomly-colored cloud. An actual image can also be given, either as an upload or as a literal url. This example displays a survey of these seed types when used to paint with the same texture parameters:

As you may note, starting from the image results in something that looks like style transfer. This isn’t so much style transfer as warping the mask image until it resembles the desired style. Among other differences, it tends to be deterministic - if you run it 3 times, you get nearly the same 3 results.

[![](http://examples.deepartist.org/img/57ebe4bf-192c-4d98-8199-1f113089d053.gif)](http://examples.deepartist.org/SeedImageSurvey/09a31f87-1322-42df-b06e-f02334275329/09a31f87-1322-42df-b06e-f02334275329.html)

### Resolution Sequence Texture Generation/Operative Resolutions
Another important factor in the painting process is what resolution we perform it at. As anyone who’s fallen from orbit knows, things look a lot different depending on how far away you are. This variety manifests itself in out painting process by the operative resolution, of what resolution we do a painting operation at. You can see the variety caused by the same painting parameters being performed at small scale, at large scale, and using intermittent stages in this example:

Growing a texture from a smaller image results in a naturally more complex and varied large structure, whereas initial painting using a higher resolution typically produces a more uniform look.

[![](http://examples.deepartist.org/img/1aa341e7-dca6-4b38-b7ca-3c8b343c14da.gif)](http://examples.deepartist.org/TextureGrowth/fe3ba3ed-a1ed-41b7-aa22-ab7bce36cd66/fe3ba3ed-a1ed-41b7-aa22-ab7bce36cd66.html)

### View Layers
This can be further modified by attaching additional filters to the vision pipeline layers, such as in these examples:

1. Tiled Texture Generation - By wrapping the canvas around itself to enlarge it, we learn to paint regardless of these wrapping boundaries, and will get a tileable image.
  [![](http://examples.deepartist.org/img/b974f273-dad3-4ee4-aa4d-4b9bdd88414a.jpg)](http://examples.deepartist.org/TiledTexture/d04858cf-eaf3-4493-9ad8-8f648efc068d/d04858cf-eaf3-4493-9ad8-8f648efc068d.html)
1. Kaleidoscope: Rotational Symmetry and Color Permutations - A layer which reflects and rotates space (and color space) can produce an image with a guaranteed symmetry. This can be combined with the tiling layer.
  [![](http://examples.deepartist.org/img/27beb20c-f858-4600-ad8b-470b982fddfe.jpg)](http://examples.deepartist.org/TextureTiledRotor/42854742-54a5-4f41-9b0a-cab3709e194d/42854742-54a5-4f41-9b0a-cab3709e194d.html)    

Additionally, the resulting canvas can be post-processed by any other function. One fun example of this is the stereogram generator.

[![](http://examples.deepartist.org/img/6d1e04c3-13d0-458a-af7c-cea5c851af10.jpg)](http://examples.deepartist.org/TextureStereogram/48a3b281-c959-4406-aafa-003863226f66/48a3b281-c959-4406-aafa-003863226f66.html)

## Style Transfer
Depending on your religion, you may wish to paint a painting to resemble some object or scene. If you insist on your graven images, you can use the mask matching operators. These are handled slightly differently than the style operators, but are fully illustrated by the following examples.

### Content Reconstruction Survey
Content images can be reconstructed by signal matching using any vision layer, in the same manner as style matching. Each level higher conveys less local information, such as color, and more higher-level information such as patterns. You can see this in the following survey, which uses each layer in a pipeline to reconstruct target mask without any further modifiers.

[![](http://examples.deepartist.org/img/678936dc-a71d-4cce-b4c6-90f43482c852.gif)](http://examples.deepartist.org/ContentReconstructionSurveyEC2/259d1a46-4a18-43d9-bf06-e8f07b45c6ac/259d1a46-4a18-43d9-bf06-e8f07b45c6ac.html)

### Simple Style Transfer
When these mask operators are combined with style operators, we can demonstrate classic deep style transfer, such as in this example:

[![](http://examples.deepartist.org/img/cab5c532-4dac-46a1-bb67-02bd154ea837.jpg)](http://examples.deepartist.org/StyleTransfer/333d3514-d2a9-467e-b157-6d9d20969bb6/333d3514-d2a9-467e-b157-6d9d20969bb6.html)

### High-resolution Multi-phase style transfer
For high-definition results, multiple phases of style transfer processes can be used while progressively enlarging the canvas. By fine-tuning the parameters for each resolution, we can gain better control over the result.

[![](http://examples.deepartist.org/img/8e95e07f-ff25-4416-9f6c-6dc5c1ccfdc0.jpg)](http://examples.deepartist.org/HighResStyleTransfer/a9f0b3ab-4487-4cea-9032-07fea1fa900b/a9f0b3ab-4487-4cea-9032-07fea1fa900b.html)

### Animations
There are images, there are videos, and in between there are animated gifs. Animated GIFs are one speciality of DeepArtist.org.

### Surveys
A variety of animations have already been shown, wherein we survey a variety of treatments, and combine them with labels into an animation.

### Sweeps
Another type of animation is to sweep a range of parameters. One example provided is a style transfer sweep from one style to another:

[![](http://examples.deepartist.org/img/1ffd8be3-baaa-47a0-b400-17c0a359e324.gif)](http://examples.deepartist.org/StyleTransferSweep/51a88a4d-70c2-4687-8bd8-fd3abed2be07/51a88a4d-70c2-4687-8bd8-fd3abed2be07.html)

### Determinism and Jitter
Why start with white noise when we do a style transfer? Wouldn’t it be faster to start with the mask image itself and change it to match the desired style? One reason is that this heavily biases the result in a way you will have trouble controlling, but another is that the result is deterministic. If we start with noise, there is an inherent randomness to our resulting image that makes it unique. If we use this randomness to produce an animation, we get a unique jittery effect:

[![](http://examples.deepartist.org/img/3be18c8e-308e-4fc3-9a99-2f520c303504.gif)](http://examples.deepartist.org/AnimatedStyleTransfer/8cc2213a-47e2-4f66-aba2-94c445b61efc/8cc2213a-47e2-4f66-aba2-94c445b61efc.html)

This can also be combined with kaleidoscopic textures:

[![](http://examples.deepartist.org/img/aeefd22c-4163-4011-b90e-13df76d98d92.gif)](http://examples.deepartist.org/AnimatedRotor/6de772c2-8b5f-465b-a816-35d60b6695f8/6de772c2-8b5f-465b-a816-35d60b6695f8.html)



